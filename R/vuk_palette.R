#' get_vuk_palette
#'
#' Function to return color pallete using VUK visual identity, maximum is 8 colors
#' @param int n colors to return
#' @keywords hex color pallete
#' @export
#' @returns palette with n desired colors as named vector
#' @examples
#' get_vuk_palette()
get_vuk_palette <- function(n) {
  palette_hex <- c('#d73526', '#efd621', '#dbce10', '#95c11f', '#3aaa35', '#85cee4', '#bce4fa', '#6a509d', "#140302")
  names(palette_hex) <- c('VUK_cervena',
                          'VUK_zluta_1',
                          'VUK_zluta_2',
                          'VUK_zelena_1',
                          'VUK_zelena_2',
                          'VUK_modra_2',
                          'VUK_modra_1',
                          'VUK_fialova',
                          "VUK_RichBlack"
                          )
  
  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    stop("Parametr n musí být kladné celé číslo.")
  }
  if (n > length(palette_hex)) {
    stop("Požadovaný počet barev je větší než počet dostupných ve vzorníku!")
  }
  
  # Převod do Lab: convertColor očekává matici N x 3 (proto t(col2rgb(...)) / 255)
  lab <- grDevices::convertColor(
    t(grDevices::col2rgb(palette_hex)) / 255,
    from = "sRGB", to = "Lab", scale.in = 1
  )
  
  # Eukleidovská vzdálenost v Lab mezi řádky i a j
  pairdist <- function(i, j) sqrt(sum((lab[i, ] - lab[j, ])^2))
  
  # 1) První barva = max průměrná vzdálenost k ostatním
  avg_d <- sapply(seq_len(nrow(lab)), function(i) {
    mean(sapply(setdiff(seq_len(nrow(lab)), i), function(j) pairdist(i, j)))
  })
  chosen_idx <- c(which.max(avg_d))
  
  # 2) Farthest-first: vždy přidáme barvu s max minimální vzdáleností k již vybraným
  while (length(chosen_idx) < n) {
    cand <- setdiff(seq_len(nrow(lab)), chosen_idx)
    cand_scores <- sapply(cand, function(i) {
      min(sapply(chosen_idx, function(j) pairdist(i, j)))
    })
    next_id <- cand[which.max(cand_scores)]
    chosen_idx <- c(chosen_idx, next_id)
  }
  
  palette_hex[chosen_idx]
}

#' show_vuk_palette
#'
#' Function to return color pallete using VUK visual identity, maximum is 8 colors

#' @keywords show VUK palette
#' @export
#' #' @returns all palette as named vector
#' @examples
#' show_vuk_palette()
show_vuk_palette <- function(n) {
  palette_hex <- c('#d73526', '#efd621', '#dbce10', '#95c11f', '#3aaa35', '#85cee4', '#bce4fa', '#6a509d',"#140302" )
  names(palette_hex) <- c('VUK_cervena',
                          'VUK_zluta_1',
                          'VUK_zluta_2',
                          'VUK_zelena_1',
                          'VUK_zelena_2',
                          'VUK_modra_2',
                          'VUK_modra_1',
                          'VUK_fialova',
                          "VUK_RichBlack")
  barplot(rep(1, length(palette_hex)),
          col = palette_hex,
          border = NA,
          axes = FALSE,
          names.arg = names(palette_hex),
          las=2, cex.names = 0.7
          )
  
  return(palette_hex)
}
