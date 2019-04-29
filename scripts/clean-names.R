nms19 <- c("county", "distid", "dist_name", "instid", "inst_name", 
           "inst_type", "stu_type", "stu_group", "selfreg_score", 
           "interpersonal_score", "atl_score", "atl_n", "math_score", "math_n",
           "upperln_score", "upperln_n", "lowerln_score", "lowerln_n",
           "ls_score", "ls_n")
nms18 <- nms19
nms17 <- c(nms18[1:14], "spanish_score", "spanish_n", nms18[15:length(nms18)])
nms16 <- c(nms17[1:14], 
           paste(rep(c("ln", "ls", "spanish"), each = 2), 
                 c("score", "n"), 
                 sep = "_"))
nms15 <- nms16
nms14 <- nms15[-grep("spanish", nms15)]

nms <- list(nms14, nms15, nms16, nms17, nms18, nms19)
names(nms) <- english(14:19)
