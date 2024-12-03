#' Reverse Complement Translation
#'
#' Translates the reverse complement of a DNA sequence into a protein sequence.
#' @param dna_seq A character string representing a DNA sequence.
#' @return A character string representing the protein sequence.
#' @examples
#' reverse_translate("ATGCGT")
#' @export
reverse_translate <- function(dna_seq) {
  complement <- chartr("ACGT", "TGCA", dna_seq)
  reverse_complement <- paste(rev(strsplit(complement, NULL)[[1]]), collapse = "")
  translate_basic(reverse_complement)
}
