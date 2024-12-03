#' Basic DNA to Protein Translation
#'
#' Translates a DNA sequence into a protein sequence.
#' @param dna_seq A character string representing a DNA sequence (e.g., "ATGCGT").
#' @return A character string representing the protein sequence.
#' @examples
#' translate_basic("ATGCGT")
#' @export
translate_basic <- function(dna_seq) {
  dna_seq <- toupper(dna_seq)
  codon_table <- list(
    "ATA" = "I", "ATC" = "I", "ATT" = "I", "ATG" = "M",
    "ACA" = "T", "ACC" = "T", "ACG" = "T", "ACT" = "T",
    "AAC" = "N", "AAT" = "N", "AAA" = "K", "AAG" = "K",
    "AGC" = "S", "AGT" = "S", "AGA" = "R", "AGG" = "R",
    "CTA" = "L", "CTC" = "L", "CTG" = "L", "CTT" = "L",
    "CCA" = "P", "CCC" = "P", "CCG" = "P", "CCT" = "P",
    "CAC" = "H", "CAT" = "H", "CAA" = "Q", "CAG" = "Q",
    "CGA" = "R", "CGC" = "R", "CGG" = "R", "CGT" = "R",
    "GTA" = "V", "GTC" = "V", "GTG" = "V", "GTT" = "V",
    "GCA" = "A", "GCC" = "A", "GCG" = "A", "GCT" = "A",
    "GAC" = "D", "GAT" = "D", "GAA" = "E", "GAG" = "E",
    "GGA" = "G", "GGC" = "G", "GGG" = "G", "GGT" = "G",
    "TCA" = "S", "TCC" = "S", "TCG" = "S", "TCT" = "S",
    "TTC" = "F", "TTT" = "F", "TTA" = "L", "TTG" = "L",
    "TAC" = "Y", "TAT" = "Y", "TAA" = "*", "TAG" = "*",
    "TGC" = "C", "TGT" = "C", "TGA" = "*", "TGG" = "W"
  )
  codons <- substring(dna_seq, seq(1, nchar(dna_seq), by = 3), seq(3, nchar(dna_seq), by = 3))
  protein <- sapply(codons, function(codon) codon_table[[codon]] %||% "-")
  paste0(protein, collapse = "")
}
