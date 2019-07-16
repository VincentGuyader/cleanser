# from https://github.com/hrbrmstr/simplemagic
get_content_type <- function(path) {
  path <- path.expand(path)
  if (!file.exists(path)) stop("File not found.", call.=FALSE)
  
  hdr <- readBin(path, "raw", n=1024)
  if (all(c(0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00, 0x00, 0x01, 0x69, 
            0x22) == hdr[1:10])) {
    return('application/octet-stream')
  }
  if (all(c(0x58, 0x0a, 0x00, 0x00, 0x00, 0x02, 0x00, 0x03, 0x04, 
            0x03) == hdr[1:10])) {
    return('application/octet-stream')
  }
  if (all(c(0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
            0x06) == hdr[1:10])) {
    return('application/octet-stream')
  }
  if (all(c(0x42, 0x5a, 0x68, 0x39, 0x31, 0x41, 0x59, 0x26, 0x53, 
            0x59) == hdr[1:10])) {
    return('application/octet-stream')
  }
  
  
  
  if (all(c(0xD0,0xCF,0x11,0xE0,0xA1,0xB1,0x1A,0xE1) == hdr[1:8])) {
    guessed_name <- guess_content_type(path)
    if ((length(guessed_name) == 1) && (guessed_name != "???")) return(guessed_name)
    return("application/msword")
  }
  
  
  if (hdr[1] == 0x3c) { # "<"
    if (all(c(0x68,0x74,0x6d,0x6c) == hdr[2:5])) return("text/html") # "html"
    if (all(c(0x48,0x54,0x4d,0x4c) == hdr[2:5])) return("text/html") # "HTML"
    if (all(c(0x48,0x45,0x41,0x44) == hdr[2:5])) return("text/html") # "HEAD"
    if (all(c(0x68,0x65,0x61,0x64) == hdr[2:5])) return("text/html") # "head"
    if (all(c(0x3f,0x78,0x6d,0x6c,0x20) == hdr[2:6])) return("application/xml")
  }
  
  if (all(c(0xfe,0xff) == hdr[1:2])) {
    if (all(c(0x00,0x3c,0x00,0x3f,0x00,0x78) == hdr[3:8])) return("application/xml")
  }
  
  
  
  if (all(c(0x50, 0x4b) == hdr[1:2])) { # "PK"
    
    office_type <- check_office(hdr, path)
    if (length(office_type) > 0) return(office_type)
    
    guessed_name <- guess_content_type(path)
    if ((length(guessed_name) == 1) && (guessed_name != "???")) return(guessed_name)
    
    return("application/zip")
    
  }
  
  return(guess_content_type(path))
  
}

check_office <- function(hdr, path) 
{
  pat_content_types <- c(91, 67, 111, 110, 116, 101, 110, 
                         116, 95, 84, 121, 112, 101, 115, 93, 46, 120, 109, 108)
  pat_rels <- c(95, 114, 101, 108, 115, 47, 46, 114, 101, 
                108, 115)
  if ((all(pat_content_types == hdr[31:49])) || (all(pat_rels == 
                                                     hdr[31:41]))) {
    # hdr <- readBin(path, "raw", n = 4096)
    # pat_word <- c(119, 111, 114, 100, 47)
    # if (length(seq_in(hdr, pat_word)) > 0) 
    #   return("application/vnd.openxmlformats-officedocument.wordprocessingml.document")
    # pat_ppt <- c(112, 112, 116, 47)
    # if (length(seq_in(hdr, pat_ppt)) > 0) 
    #   return("application/vnd.openxmlformats-officedocument.presentationml.presentation")
    # pat_xl <- c(120, 108, 47)
    # if (length(seq_in(hdr, pat_xl)) > 0) 
      return("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  }
  return(NULL)
}

seq_in <- function (haystack, needle) 
{
  index <- seq_along(haystack)
  for (i in seq_along(needle)) {
    pile <- haystack[index]
    index <- index[pile == needle[i]] + 1L
  }
  (index - length(needle))[1]
}
#' @importFrom tools file_ext
guess_content_type<-function (path) 
{
  path <- path.expand(path)
  if (!file.exists(path)) 
    stop("File not found.", call. = FALSE)
  extension <- trimws(tolower(file_ext(path)))
  res <- simplemagic_mime_db[(simplemagic_mime_db$extension == 
                                extension), ]$mime_type
  if (length(res) == 0) 
    return("???")
  return(unique(res))
}
