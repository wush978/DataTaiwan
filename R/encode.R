to_utf8 <- function(src) {
  enc <- stringi::stri_enc_detect(src)
  sapply(seq_along(src), function(i) {
    stringi::stri_encode(src[i],
                         from = enc[[1]]$Encoding[1],
                         to = "UTF-8")
  })
}
