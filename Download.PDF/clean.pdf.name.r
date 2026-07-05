library(data.table)
setwd("/path_to_your_project")
#dir_path<-"/path_to_your_project/Data/PDF"
dir_path<-"~/Downloads/"
files <- list.files(path = dir_path, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
dt <- data.table(path = files)

dt[, c("basename_up", "size") := .(toupper(basename(path)), file.info(path)$size)]

dt[, max_flag := size == max(size), by = basename_up]


to_remove <- dt[max_flag == FALSE, path]
if (length(to_remove) > 0) {
  file.remove(to_remove)
  message("Removed ", length(to_remove), " duplicate(s).")
}

dt_keep <- dt[max_flag == TRUE]
dt_keep[, new_path := file.path(dirname(path), basename_up)]

dt_keep[path != new_path, 
        { file.rename(path, new_path); .N }, by = path]

message("Renamed ", nrow(dt_keep[path != new_path]), " file(s) to uppercase.")
