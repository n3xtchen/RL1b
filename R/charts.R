library("ggplot2")

basicChart  <- function(data, name, color='pink', dateBreaks="1 day", yBreaks=10, show.axis.x=F, show.avg.line=T, show.info.text=T) {
  lastest <- data[nrow(data),]
  avg <- mean(data[,name])
  rate <- round(((lastest[,name]/mean(data[,name]))-1)*100,2)
  y = c(seq(min(data[,name], na.rm=T)-2*yBreaks, max(data[,name], na.rm=T)+2*yBreaks, yBreaks))

  p <- ggplot(data, aes_string(x='date', y=paste0("`", name, "`"))) + geom_line(colour=color) +
    coord_cartesian(ylim = c(min(y), max(y))) +
    geom_text(aes_string(label = paste0("`", name, "`"), vjust = 1.1, hjust = 0.5, angle = 45), show.legend = FALSE) +
    theme(axis.text.x=element_text(face="italic", size=10, angle=45, color="red", hjust=1), # x 标签倾斜
          axis.title.y = element_text(family='STKaiti'),
          legend.text=element_text(family='STKaiti', size=14), # 中文出现方块
          legend.position = "top", legend.box = "horizontal") # 图例置于顶部，水平铺开

  if (show.axis.x) {
    # 显示X轴
    p <- p + scale_x_date(date_labels="%Y-%m-%d", date_breaks=dateBreaks)  # 格式 x 轴，前提 x 轴必须是 as.Date 类型，时间间隔
  } else {
    p <- p + theme(axis.text.x = element_blank()) +
      xlab("")
  }
  if (show.avg.line) {
    # 显示均值线
    y <- c(y, avg)
    p <- p + 
      geom_hline(aes(yintercept=as.numeric(avg)),linetype=5)
  }
  if (show.info.text) {
    v <- round(avg, 2)
    vv <- as.numeric(rate)
    yy <- round(as.numeric(lastest[,name]), 2)
    p <- p + 
      annotate("text", x = as.Date(lastest[,"date"])+2, y = avg, #  + ifelse(yy>max(v),-4*yBreaks, 6*yBreaks),
               label = paste0(name,":    \n",
                              "最新: ", yy, "\n",
                              "均值: ", v, "\n",
                              ifelse(vv>=0, "上涨", "下跌")," ",as.character(abs(vv))," %    "), family="STKaiti", color="black", )
  }

  p + scale_y_continuous(breaks=sort(y))  # 设置 y 轴刻度
}
