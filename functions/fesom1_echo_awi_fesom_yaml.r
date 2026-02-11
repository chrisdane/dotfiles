#!/usr/bin/env Rscript

# just print stuff

cmd <- ("echo $AWI_FESOM_YAML")
message("run `", cmd, "` ...")
val <- system(cmd, intern=T)
message("AWI_FESOM_YAML = ", val)

if (val == "") {
    stop("--> run `export AWI_FESOM_YAML = ...` first") 
} else {
    val <- sub("\\{output_schedules: \\[", "", val)
    val <- substr(val, 1, nchar(val)-2)
    val <- strsplit(val, "\\{vars: ")[[1]]
    val <- val[2:length(val)]
    message("--> there are ", length(val), " frequency blocks:")
    for (vi in seq_along(val)) {
        message("**************************************************")
        message(val[vi])
    }
}


