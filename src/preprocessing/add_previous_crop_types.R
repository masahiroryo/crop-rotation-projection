add_prev_crop_type <- function(dat){
  pctype <- dat$CType
  pctype[seq(0, nrow(dat), by=16)] <- NA
  pctype <- c(NA, pctype[-length(pctype)])
  return(pctype)
}

add_prev_prev_crop_type <- function(dat){
  ppctype <- dat$CType
  ppctype[sort(c(seq(0, nrow(dat), by=16), seq(-1, nrow(dat)-1, by=16)))[-c(1:2)]] <- NA
  ppctype <- c(NA, NA, ppctype[-c(length(ppctype)-1, length(ppctype))])
  return(ppctype)
}