sm_auc_list <- function(subjects, conditions, x, values,data) {
  
  x <- unique(data[[x]])
  subjects_list <- unique(as.character(data[[subjects]]))
  subj_num <- length(subjects_list)
  cond_list <- unique(data[[conditions]])
  cond_num <- length(cond_list)
  x_length <- length(unique(x))
  
  auc_list <- data.frame(matrix(ncol = 3, nrow = subj_num*cond_num))
  names(auc_list) <- c(subjects, conditions, paste0('AUC_', values))
  
  for (iCond in seq_along(1:cond_num)) {
    for (iSubj in seq_along(1:subj_num)) {
      ind <- which(data[[conditions]] == unique(cond_list)[iCond] &
                     data[[subjects]] == unique(subjects_list)[iSubj])
      
      auc_list[,1][(cond_num*(iSubj-1))+(iCond)] <- subjects_list[iSubj]
      auc_list[,2][(cond_num*(iSubj-1))+(iCond)] <- cond_list[iCond]
      auc_list[,3][(cond_num*(iSubj-1))+(iCond)] <- sm_auc(x,data[[values]][ind])
    }
  }
  return(auc_list)
}
