rm(list=ls())
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")

## MOVE
#MCP
AIC_move_all_humans <- read.csv(file = "PC1_AIC_all_points.move_MCP_closest_M_.csv")
AIC_move_humans <- read.csv(file = "PC1_AIC_all_points.move_MCP_closest2_M_.csv")

AIC_move_humans[,2:5] <- round(AIC_move_humans[,2:5], digits=2)
AIC_move_all_humans[,2:5] <- round(AIC_move_all_humans[,2:5], digits=2)

AIC_move_all_humans[nrow(AIC_move_all_humans)+1,2] <- c("Distance humans")
AICs_MCP <- rbind( AIC_move_all_humans, AIC_move_humans)

#KERN
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
AIC_move_all_humans_K <- read.csv(file = "PC1_AIC_all_points.move_KERN_closest_M_.csv")
AIC_move_humans_K <- read.csv(file = "PC1_AIC_all_points.move_KERN_closest2_M_.csv")

AIC_move_humans_K[,2:5] <- round(AIC_move_humans_K[,2:5], digits=2)
AIC_move_all_humans_K[,2:5] <- round(AIC_move_all_humans_K[,2:5], digits=2)

AIC_move_all_humans_K[nrow(AIC_move_all_humans_K)+1,2] <- c("Dist_humans")
AICs_K <- rbind(AIC_move_all_humans_K, AIC_move_humans_K)

AIC_MOVE <- cbind(AICs_MCP,AICs_K)

setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures/Final")
write.csv(AIC_MOVE,file ="AIC_MCP_KERN_MOVE.csv")


## NOTMOVE
rm(list=ls())
#MCP
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures")
AIC_NOT_move_all_humans_MCP <- read.csv(file = "PC1_AIC_all_points.not.moved_MCP_closest_M_.csv")
AIC_NOT_move_humans_MCP <- read.csv(file = "PC1_AIC_all_points.not.moved_MCP_closest2_M_.csv")

AIC_NOT_move_humans_MCP[,2:5] <- round(AIC_NOT_move_humans_MCP[,2:5], digits=2)
AIC_NOT_move_all_humans_MCP[,2:5] <- round(AIC_NOT_move_all_humans_MCP[,2:5], digits=2)

AIC_NOT_move_all_humans_MCP[nrow(AIC_NOT_move_all_humans_MCP)+1,2] <- c("Dist_humans")
AICs_MCP <- rbind(AIC_NOT_move_all_humans_MCP, AIC_NOT_move_humans_MCP)

#KERN
AIC_NOT_move_all_humans_K <- read.csv(file = "PC1_AIC_all_points.not.moved_KERN_closest_M_.csv")
AIC_NOT_move_humans_K <- read.csv(file = "PC1_AIC_all_points.not.moved_KERN_closest2_M_.csv")

AIC_NOT_move_humans_K[,2:5] <- round(AIC_NOT_move_humans_K[,2:5], digits=2)
AIC_NOT_move_all_humans_K[,2:5] <- round(AIC_NOT_move_all_humans_K[,2:5], digits=2)

AIC_NOT_move_all_humans_K[nrow(AIC_NOT_move_all_humans_K)+1,2] <- c("Dist_humans")
AICs_K <- rbind(AIC_NOT_move_all_humans_K, AIC_NOT_move_humans_K)


AIC_NOT_MOVED <- cbind(AICs_MCP,AICs_K)
setwd("C:/Personal_Cloud/OneDrive/Work/Skandulv/NHBD2/figures/Final")
write.csv(AIC_NOT_MOVED,file ="AIC_MCP_KERN_NOT_MOVE.csv")


