use std::io::{BufReader, Read, Seek, SeekFrom};
use byteorder::{LittleEndian, ReadBytesExt};
use chess::{Board, BitBoard, Color, Piece, Square};
use once_cell::sync::OnceCell;
use std::fmt;
use std::error::Error;
use std::io::Cursor;

#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

const FEATURE_TRANSFORMER_HALF_DIMENSIONS: usize = 256;
const SQUARE_NB: usize = 64;
const FT_INPUT_DIM: usize = 41024;
const HL1_INPUT_DIM: usize = 512; 
const HL1_OUTPUT_DIM: usize = 32;
const HL2_OUTPUT_DIM: usize = 32;

pub struct Model {
    ft_weights: Vec<i16>,
    ft_biases: Vec<i16>,

    hl1_weights: Vec<i8>,
    hl1_biases: Vec<i32>,

    hl2_weights: Vec<i8>,
    hl2_biases: Vec<i32>,

    out_weights: Vec<i8>,
    out_bias: i32,
}

static MODEL: OnceCell<Model> = OnceCell::new();

#[derive(Debug)]
pub enum NnueError {
    IoError(std::io::Error),
    ValueError(String),
    AlreadyInitialized,
}

impl fmt::Display for NnueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NnueError::IoError(e) => write!(f, "I/O Error: {}", e),
            NnueError::ValueError(msg) => write!(f, "Value Error: {}", msg),
            NnueError::AlreadyInitialized => write!(f, "Modell bereits initialisiert!"),
        }
    }
}

impl Error for NnueError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            NnueError::IoError(e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::io::Error> for NnueError {
    fn from(e: std::io::Error) -> Self {
        NnueError::IoError(e)
    }
}

pub fn init_nnue(model_data: &[u8]) -> Result<(), NnueError> {
    let mut reader = BufReader::new(Cursor::new(model_data)); // Benutze den eingebetteten Byte-Daten

    let version = reader.read_u32::<LittleEndian>()?;
    let hash_value = reader.read_u32::<LittleEndian>()?;
    let size = reader.read_u32::<LittleEndian>()? as usize;
    let mut arch_bytes = vec![0u8; size];
    reader.read_exact(&mut arch_bytes)?;
    let _arch = String::from_utf8_lossy(&arch_bytes);
    println!("Version: {}", version);
    println!("Hash: {}", hash_value);

    let ft_header = reader.read_u32::<LittleEndian>()?;
    let associated_halfkp_king: u32 = 1;
    let output_dimensions = 2 * FEATURE_TRANSFORMER_HALF_DIMENSIONS as u32;
    let expected_hash = (0x5D69D5B9_u32 ^ associated_halfkp_king) ^ output_dimensions;
    if ft_header != expected_hash {
        return Err(NnueError::ValueError("Header passt nicht zum erwarteten Hash!".to_string()));
    }

    let mut ft_biases = vec![0i16; FEATURE_TRANSFORMER_HALF_DIMENSIONS];
    reader.read_i16_into::<LittleEndian>(&mut ft_biases)?;

    let ft_weights_count = FEATURE_TRANSFORMER_HALF_DIMENSIONS * FT_INPUT_DIM;
    let mut ft_weights = vec![0i16; ft_weights_count];
    reader.read_i16_into::<LittleEndian>(&mut ft_weights)?;

    let _header = reader.read_u32::<LittleEndian>()?;
    let mut hl1_biases = vec![0i32; HL1_OUTPUT_DIM];
    reader.read_i32_into::<LittleEndian>(&mut hl1_biases)?;

    let hl1_weights_count = HL1_INPUT_DIM * HL1_OUTPUT_DIM;
    let mut hl1_weights = vec![0i8; hl1_weights_count];
    reader.read_exact(unsafe {
        std::slice::from_raw_parts_mut(
            hl1_weights.as_mut_ptr() as *mut u8,
            hl1_weights_count * std::mem::size_of::<i8>(),
        )
    })?;

    let mut hl2_biases = vec![0i32; HL2_OUTPUT_DIM];
    reader.read_i32_into::<LittleEndian>(&mut hl2_biases)?;

    let hl2_weights_count = HL2_OUTPUT_DIM * HL2_OUTPUT_DIM;
    let mut hl2_weights = vec![0i8; hl2_weights_count];
    reader.read_exact(unsafe {
        std::slice::from_raw_parts_mut(
            hl2_weights.as_mut_ptr() as *mut u8,
            hl2_weights_count * std::mem::size_of::<i8>(),
        )
    })?;

    let out_bias = reader.read_i32::<LittleEndian>()?;
    let mut out_weights = vec![0i8; HL2_OUTPUT_DIM];
    reader.read_exact(unsafe {
        std::slice::from_raw_parts_mut(
            out_weights.as_mut_ptr() as *mut u8,
            HL2_OUTPUT_DIM * std::mem::size_of::<i8>(),
        )
    })?;

	let current_pos = reader.seek(SeekFrom::Current(0))?;
	let end_pos = model_data.len();
	if end_pos - current_pos as usize != 0 {
		return Err(NnueError::ValueError("Es wurden nicht alle Parameter gelesen!".to_string()));
	}
    let model = Model {
        ft_weights,
        ft_biases,
        hl1_weights,
        hl1_biases,
        hl2_weights,
        hl2_biases,
        out_weights,
        out_bias,
    };

    MODEL.set(model).map_err(|_| NnueError::AlreadyInitialized)?;
    Ok(())
}

pub fn eval_nnue(fen: &str) -> Result<f32, NnueError> {
    let model = MODEL.get().ok_or_else(|| {
        NnueError::ValueError("Modell nicht initialisiert! Bitte zuerst init_nnue(nnue_path) aufrufen.".to_string())
    })?;

    let board = Board::from_fen(fen.to_string())
        .ok_or_else(|| NnueError::ValueError("Ungültiger FEN-String".to_string()))?;

    let features_current = get_halfkp_indices(&board, board.side_to_move() == Color::White);
    let features_opponent = get_halfkp_indices(&board, board.side_to_move() == Color::Black);

    let ft_current = {
        #[cfg(target_arch = "x86_64")]
        {
            unsafe { feature_transformer_simd(&features_current, &model.ft_weights, &model.ft_biases) }
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            feature_transformer(&features_current, &model.ft_weights, &model.ft_biases)
        }
    };

    let ft_opponent = {
        #[cfg(target_arch = "x86_64")]
        {
            unsafe { feature_transformer_simd(&features_opponent, &model.ft_weights, &model.ft_biases) }
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            feature_transformer(&features_opponent, &model.ft_weights, &model.ft_biases)
        }
    };

    let mut concat_features = [0i32; HL1_INPUT_DIM];
    concat_features[..FEATURE_TRANSFORMER_HALF_DIMENSIONS].copy_from_slice(&ft_current);
    concat_features[FEATURE_TRANSFORMER_HALF_DIMENSIONS..].copy_from_slice(&ft_opponent);

    let hl1_out = dense_layer(&concat_features, &model.hl1_weights, &model.hl1_biases, HL1_INPUT_DIM, HL1_OUTPUT_DIM);
    let hl2_out = dense_layer(&hl1_out, &model.hl2_weights, &model.hl2_biases, HL2_OUTPUT_DIM, HL2_OUTPUT_DIM);
    let out_value = dense_output(&hl2_out, &model.out_weights, model.out_bias);
    let centipawn = nn_value_to_centipawn(out_value);
    Ok(centipawn)
}

fn feature_transformer(indices: &[usize], ft_weights: &[i16], ft_biases: &[i16]) -> [i32; FEATURE_TRANSFORMER_HALF_DIMENSIONS] {
    let mut out = [0i32; FEATURE_TRANSFORMER_HALF_DIMENSIONS];
    for i in 0..FEATURE_TRANSFORMER_HALF_DIMENSIONS {
        out[i] = ft_biases[i] as i32;
    }
    for &idx in indices {
        let base = idx * FEATURE_TRANSFORMER_HALF_DIMENSIONS;
        for i in 0..FEATURE_TRANSFORMER_HALF_DIMENSIONS {
            out[i] += ft_weights[base + i] as i32;
        }
    }
    for v in &mut out {
        *v = (*v).clamp(0, 127);
    }
    out
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn feature_transformer_simd(
    indices: &[usize],
    ft_weights: &[i16],
    ft_biases: &[i16],
) -> [i32; FEATURE_TRANSFORMER_HALF_DIMENSIONS] {
    let mut out = [0i32; FEATURE_TRANSFORMER_HALF_DIMENSIONS];
    for i in 0..FEATURE_TRANSFORMER_HALF_DIMENSIONS {
        out[i] = ft_biases[i] as i32;
    }
    for &idx in indices {
        let base = idx * FEATURE_TRANSFORMER_HALF_DIMENSIONS;
        let mut i = 0;
        while i + 8 <= FEATURE_TRANSFORMER_HALF_DIMENSIONS {
            let out_vec = _mm256_loadu_si256(out.as_ptr().add(i) as *const __m256i);
            let wt_ptr = ft_weights.as_ptr().add(base + i);
            let wt_i16 = _mm_loadu_si128(wt_ptr as *const __m128i);
            let wt_vec = _mm256_cvtepi16_epi32(wt_i16);
            let sum_vec = _mm256_add_epi32(out_vec, wt_vec);
            _mm256_storeu_si256(out.as_mut_ptr().add(i) as *mut __m256i, sum_vec);
            i += 8;
        }
        while i < FEATURE_TRANSFORMER_HALF_DIMENSIONS {
            out[i] += ft_weights[base + i] as i32;
            i += 1;
        }
    }
    let mut i = 0;
    let zero = _mm256_set1_epi32(0);
    let max = _mm256_set1_epi32(127);
    while i + 8 <= FEATURE_TRANSFORMER_HALF_DIMENSIONS {
        let v = _mm256_loadu_si256(out.as_ptr().add(i) as *const __m256i);
        let clamped = _mm256_min_epi32(_mm256_max_epi32(v, zero), max);
        _mm256_storeu_si256(out.as_mut_ptr().add(i) as *mut __m256i, clamped);
        i += 8;
    }
    while i < FEATURE_TRANSFORMER_HALF_DIMENSIONS {
        out[i] = out[i].clamp(0, 127);
        i += 1;
    }
    out
}

#[cfg(not(target_arch = "x86_64"))]
fn feature_transformer_simd(
    indices: &[usize],
    ft_weights: &[i16],
    ft_biases: &[i16],
) -> [i32; FEATURE_TRANSFORMER_HALF_DIMENSIONS] {
    feature_transformer(indices, ft_weights, ft_biases)
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn dot_product_avx2(input: &[i32], weights: &[i8]) -> i32 {
    let len = input.len();
    let mut i = 0;
    let chunk_size = 8; 
    let mut acc = _mm256_setzero_si256();
    let input_ptr = input.as_ptr();
    let weights_ptr = weights.as_ptr();
    let aligned = (input_ptr as usize) % 32 == 0 && (weights_ptr as usize) % 32 == 0;

    if aligned {
        while i + (2 * chunk_size) <= len {
            let in_vec1 = _mm256_load_si256(input_ptr.add(i) as *const __m256i);
            let wt_chunk1 = _mm_loadl_epi64(weights_ptr.add(i) as *const __m128i);
            let wt_vec1 = _mm256_cvtepi8_epi32(wt_chunk1);

            let in_vec2 = _mm256_load_si256(input_ptr.add(i + chunk_size) as *const __m256i);
            let wt_chunk2 = _mm_loadl_epi64(weights_ptr.add(i + chunk_size) as *const __m128i);
            let wt_vec2 = _mm256_cvtepi8_epi32(wt_chunk2);

            acc = _mm256_add_epi32(acc, _mm256_add_epi32(
                _mm256_mullo_epi32(in_vec1, wt_vec1),
                _mm256_mullo_epi32(in_vec2, wt_vec2)
            ));
            i += 2 * chunk_size;
        }
        while i + chunk_size <= len {
            let in_vec = _mm256_load_si256(input_ptr.add(i) as *const __m256i);
            let wt_chunk = _mm_loadl_epi64(weights_ptr.add(i) as *const __m128i);
            let wt_vec = _mm256_cvtepi8_epi32(wt_chunk);
            acc = _mm256_add_epi32(acc, _mm256_mullo_epi32(in_vec, wt_vec));
            i += chunk_size;
        }
    } else {
        while i + (2 * chunk_size) <= len {
            let in_vec1 = _mm256_loadu_si256(input_ptr.add(i) as *const __m256i);
            let wt_chunk1 = _mm_loadl_epi64(weights_ptr.add(i) as *const __m128i);
            let wt_vec1 = _mm256_cvtepi8_epi32(wt_chunk1);

            let in_vec2 = _mm256_loadu_si256(input_ptr.add(i + chunk_size) as *const __m256i);
            let wt_chunk2 = _mm_loadl_epi64(weights_ptr.add(i + chunk_size) as *const __m128i);
            let wt_vec2 = _mm256_cvtepi8_epi32(wt_chunk2);

            acc = _mm256_add_epi32(acc, _mm256_add_epi32(
                _mm256_mullo_epi32(in_vec1, wt_vec1),
                _mm256_mullo_epi32(in_vec2, wt_vec2)
            ));
            i += 2 * chunk_size;
        }
        while i + chunk_size <= len {
            let in_vec = _mm256_loadu_si256(input_ptr.add(i) as *const __m256i);
            let wt_chunk = _mm_loadl_epi64(weights_ptr.add(i) as *const __m128i);
            let wt_vec = _mm256_cvtepi8_epi32(wt_chunk);
            acc = _mm256_add_epi32(acc, _mm256_mullo_epi32(in_vec, wt_vec));
            i += chunk_size;
        }
    }

    let mut acc_arr = [0i32; 8];
    _mm256_storeu_si256(acc_arr.as_mut_ptr() as *mut __m256i, acc);
    let mut sum = acc_arr.iter().sum();
    while i < len {
        sum += input[i] * (weights[i] as i32);
        i += 1;
    }
    sum
}

#[cfg(not(target_arch = "x86_64"))]
fn dot_product_avx2(input: &[i32], weights: &[i8]) -> i32 {
    input.iter().zip(weights.iter()).map(|(&x, &w)| x * (w as i32)).sum()
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn dense_layer_simd(input: &[i32], weights: &[i8], bias: i32, _in_dim: usize) -> i32 {
    bias + dot_product_avx2(input, weights)
}

#[cfg(not(target_arch = "x86_64"))]
fn dense_layer_simd(input: &[i32], weights: &[i8], bias: i32, _in_dim: usize) -> i32 {
    bias + input.iter().zip(weights.iter()).map(|(&x, &w)| x * (w as i32)).sum::<i32>()
}

#[inline]
fn dense_layer(input: &[i32], weights: &[i8], biases: &[i32], in_dim: usize, out_dim: usize) -> [i32; HL1_OUTPUT_DIM] {
    let mut out = [0i32; HL1_OUTPUT_DIM];
    for j in 0..out_dim {
        let weight_slice = &weights[j * in_dim..(j + 1) * in_dim];
        let sum = unsafe { dense_layer_simd(input, weight_slice, biases[j], in_dim) };
        out[j] = nnue_relu(sum);
    }
    out
}

#[inline]
fn dense_output(input: &[i32], weights: &[i8], bias: i32) -> i32 {
    bias + input.iter()
        .zip(weights.iter())
        .map(|(&x, &w)| x * (w as i32))
        .sum::<i32>()
}

#[inline]
fn nnue_relu(x: i32) -> i32 {
    if x < 0 { 0 } else { let y = x / 64; if y > 127 { 127 } else { y } }
}

#[inline]
fn floor_div(a: i32, b: i32) -> i32 {
    if a >= 0 { a / b } else { -(( -a + b - 1 ) / b) }
}

fn nn_value_to_centipawn(nn_value: i32) -> f32 {
    let v = floor_div(nn_value, 8);
    let v = floor_div(v * 100, 208);
    v as f32
}



#[inline]
fn make_halfkp_index(is_white: bool, king_oriented: usize, sq: usize, piece: Piece, piece_color: Color) -> usize {
    orient(is_white, sq) + piece_square_from_piece(piece, piece_color, is_white) + 641 * king_oriented
}

#[inline]
fn orient(is_white: bool, sq: usize) -> usize {
    if is_white { sq } else { 63 - sq }
}

#[inline]
fn piece_square_from_piece(piece: Piece, piece_color: Color, is_white: bool) -> usize {
    let use_white = (piece_color == Color::White) == is_white;
    match (piece, use_white) {
        (Piece::Pawn, true)   => 1,
        (Piece::Knight, true) => 2 * SQUARE_NB + 1,
        (Piece::Bishop, true) => 4 * SQUARE_NB + 1,
        (Piece::Rook, true)   => 6 * SQUARE_NB + 1,
        (Piece::Queen, true)  => 8 * SQUARE_NB + 1,
        (Piece::King, true)   => 10 * SQUARE_NB + 1,
        (Piece::Pawn, false)   => 1 * SQUARE_NB + 1,
        (Piece::Knight, false) => 3 * SQUARE_NB + 1,
        (Piece::Bishop, false) => 5 * SQUARE_NB + 1,
        (Piece::Rook, false)   => 7 * SQUARE_NB + 1,
        (Piece::Queen, false)  => 9 * SQUARE_NB + 1,
        (Piece::King, false)   => 11 * SQUARE_NB + 1,
    }
}

fn get_halfkp_indices(board: &Board, is_white_pov: bool) -> Vec<usize> {
    let mut indices = Vec::with_capacity(64);
    let king_sq = board.king_square(if is_white_pov { Color::White } else { Color::Black });
    let king_oriented = orient(is_white_pov, king_sq.to_index());
    for i in 0..64 {
        let sq = unsafe { Square::new(i as u8) };
        if let Some(piece) = board.piece_on(sq) {
            if piece == Piece::King { continue; }
            let piece_color = if board.color_combined(Color::White) & BitBoard::from_square(sq) != BitBoard::new(0) {
                Color::White
            } else {
                Color::Black
            };
            let idx = make_halfkp_index(is_white_pov, king_oriented, sq.to_index(), piece, piece_color);
            indices.push(idx);
        }
    }
    indices
} 