rm(list = ls())

library(readxl)
library(ggplot2)
library(mytools)
library(scales)
path_dat <- "./metadata/01_rmsf_rel.csv"
# path_csv <- "./metadata/01_rmsf.csv"
# path_excel <- "./metadata/01_rmsf.xlsx"
# path_txt <- "./metadata/01_rmsf.txt"
# dat <- read.table(path_dat)
# write.csv(dat,file = "./metadata/01_rmsf.csv",row.names = F,quote = F)
# write.table(dat,file = "./metadata/01_rmsf.txt",row.names = F,quote = F)

dat_read_fun_01 <- function(path1){
  filetype_split <- unlist(strsplit(path1,split = "[.]"))
  filetype <- filetype_split[length(filetype_split)]
  dat <- switch (filetype,
    "dat" = read.table(path1,header = F),
    "csv" = read.csv(path1,header = F),
    "xlsx" = as.data.frame(readxl::read_excel(path1,sheet = 1,col_names = F)),
    "xls" = as.data.frame(readxl::read_excel(path1,sheet = 1,col_names = F)),
    "txt" = read.table(path1,header = F)
  )
  for (i in 1:ncol(dat)) {
    dat[,i] <- as.numeric(dat[,i])
  }
  dat <- na.omit(dat)
  rownames(dat) <- 1:nrow(dat)
  colnames(dat) <- c("res1","value")
  return(dat)
}
# dat_txt <- dat_read_fun_01(path_txt)
# dat_dat <- dat_read_fun_01(path_dat)
# dat_csv <- dat_read_fun_01(path_csv)
# dat_excel <- dat_read_fun_01(path_excel)
midpoint <- 0
half_range <- 0.5
fold <- 50 / half_range
limit_lower <- -50
limit_upper <- 50
color <- c("navy","white","firebrick3")
color_gradient <- colorRampPalette(color)(101)

dat <- dat_read_fun_01(path1 = path_dat)

dat$color_index <- round((dat$value-midpoint)*fold)
dat[which(dat$color_index > limit_upper),"color_index"] <- limit_upper
dat[which(dat$color_index < limit_lower),"color_index"] <- limit_lower
dat$color_index <- dat$color_index+51
dat$color <- color_gradient[dat$color_index]
cols_rgb <- as.data.frame(t(col2rgb(dat$color)))
dat$red <- cols_rgb$red
dat$green <- cols_rgb$green
dat$blue <- cols_rgb$blue
dat$color_pymol <- paste0("[",dat$red,",",dat$green,",",dat$blue,"]")

cmd_pymol <- data.frame(cmd1 = paste0("select resi ",dat$res1,";"),
                        cmd2 = paste0("set_color color",dat$res1,",",dat$color_pymol,";"),
                        cmd3 = paste0("color color",dat$res1,",sele;"))
paste0(cmd_pymol[1,],collapse = " ")

cmd_chimerax <- data.frame(cmd1 = paste0("color :",dat$res1," ",dat$color,";"))
# cmd_chimerax2 <- data.frame(cmd1 = paste0("select :",dat$res1,";"),
#                             cmd2 = paste0("color sel ",dat$color,";"))
paste0(cmd_chimerax[1,],collapse = " ")
write.table(cmd_chimerax,file = "./plots/01_cmd_chimerax.cxc",quote = F,sep = " ",row.names = F,col.names = F)
# write.table(cmd_chimerax2,file = "./plots/01_cmd_chimerax2.cxc",quote = F,sep = " ",row.names = F,col.names = F)
write.table(cmd_pymol,file = "./plots/01_cmd_pymol.pml",quote = F,sep = " ",row.names = F,col.names = F)

# colorbar
n_break <- 9
label <- seq(-half_range,half_range,length.out=101)
breaks <- label[seq(1,length(label),length.out=n_break)]
dat_bar <- data.frame(label = label)
ggplot(dat_bar,aes(x=label,y=1))+
  geom_tile(aes(fill=label))+
  scale_fill_gradient2(low = color_gradient[1],mid = color_gradient[51],high = color_gradient[101],midpoint = midpoint,
                       limits=c(-half_range,half_range),oob=squish,breaks=c(-half_range,midpoint,half_range))+
  scale_x_continuous(expand = c(0,0),breaks = breaks)+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black",linewidth = 0.5,fill = NA),
        plot.margin = margin(20,20,20,20))+
  labs(x=NULL,y=NULL)
out_plots(filename = "01_colorbar",width = 6,height = 1)

ggplot(dat_bar,aes(y=label,x=1))+
  geom_tile(aes(fill=label))+
  scale_fill_gradient2(low = color_gradient[1],mid = color_gradient[51],high = color_gradient[101],midpoint = midpoint,
                       limits=c(-half_range,half_range),oob=squish)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),sec.axis = sec_axis(~.,breaks =  breaks))+
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        panel.border = element_rect(colour = "black",linewidth = 0.5,fill = NA),
        plot.margin = margin(20,20,20,20))+
  labs(x=NULL,y=NULL)
out_plots(filename = "01_colorbar_vertical",width = 1.5,height = 6)
