# 生成报表图片
#
# 功能:
#   * 设置颜色
#   * 支持中文
#   * 自适应最大宽度
#   * debug 模式
#
# todos：
#   * 字体列高
#   * 可设置最大宽度, 对于过长字符自动换行
#

# 我写的
plot.table <- function(d, colors, margin.color, title="", text.cex=1.0, cell.width=4,  cell.height=1, debug=F) {
  par(mar=c(0,0,1,0), family="STKaiti") 
  t <- if (debug) "s" else "n"
  col.length <- ncol(d)
  row.length <- nrow(d)
  
  width.x <- c(0)
  width.cx <- c()
  for (c in 1:col.length) {
    left <- width.x[c]
    row.max.width <- max(nchar(c(c(colnames(d)[c]), d[,c])))*text.cex
    right <- left + if (row.max.width > cell.width) row.max.width else cell.width
    mid <- left + (right-left)/2
    width.x <- c(width.x, c(right))
    width.cx <- c(width.cx, c(mid))
  }
 
  width <- max(width.x)
  height <- cell.height * (row.length + 1)
  
  width.y <- c(height-cell.height)
  width.cy <- c()
  for (r in 1:nrow(d)) {
    top <- width.y[r]
    bottom <- top - cell.height
    mid <- top - cell.height*0.5
    width.y <- c(width.y, c(bottom))
    width.cy <- c(width.cy, c(mid))
  }
 
  par(mar=c(0,0,1,0), family="STKaiti") 
  plot(c(-cell.width, width), c(0, height), type="n", xaxt=t, yaxt=t,  xlab="",ylab="",main=title, bty="n")
  # row name
  for (c in 1:col.length) {
    rect(width.x[c], height-cell.height, width.x[c+1], height, col=margin.color)
    text(width.cx[c],height-cell.height/2,colnames(d)[c], cex=text.cex)
  }
  # col name
  for (r in 1:nrow(d)) {
    rect(-cell.width, width.y[r+1], 0, width.y[r], col=margin.color)
    text(-cell.width/2, width.cy[r],rownames(d)[nrow(d) - r + 1], cex=text.cex)
  }
  # data
  for (r in 1:nrow(d)) {
    for (c in 1:ncol(d)) {
      rect(width.x[c], height-cell.height*(r+1), width.x[c+1], height-cell.height*r, col=colors[nrow(d) - r + 1,c])
      text(width.cx[c], width.cy[r],d[nrow(d) - r + 1,c], cex=text.cex)
    }
  }
}

# 借鉴来的
# https://elliotnoma.wordpress.com/2015/07/18/how-to-plot-a-table-of-values-in-r/
plot_table <- function(d, colors, marginColor,main="", text.cex=1.0) {
  cell.width <- 2
  cell.height <- 1
  col.length <- ncol(d)
  row.length <- nrow(d)

  width <- cell.width * col.length
  height <- cell.height * row.length

  plot(c(-cell.width, width), c(0, height+1), type="n", xaxt="n", yaxt="n", xlab="",ylab="",main=main, bty="n")
  i <- 0
  for (c in 1:col.length) {
    rect(i, nrow(d), i+cell.width, nrow(d) + 1, col=marginColor)
    text(c-.5,nrow(d) +.5,colnames(d)[c], cex=text.cex+.2)
  }
  for (r in 1:nrow(d)) {
    rect(-1, r-1, 0, r, col=marginColor)
    text(-.5, r-.5,rownames(d)[nrow(d) - r + 1], cex=text.cex)
  }
  for (r in 1:nrow(d))
    for (c in 1:ncol(d)) {
      rect(c-1, r-1, c, r, col=colors[nrow(d) - r + 1,c])
      text(c-.5,r-.5,d[nrow(d) - r + 1,c], cex=text.cex)
    }
}

