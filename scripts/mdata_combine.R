while(min(mdata$number) < 100){
  i <- tail(which(mdata$number == min(mdata$number)), 1)
  mdata[i-1]$ratio <- (mdata[i-1]$ratio * mdata[i-1]$number +
    mdata[i]$ratio * mdata[i]$number) / (mdata[i-1]$number + mdata[i]$number)
  mdata[i-1]$number <- mdata[i-1]$number + mdata[i]$number
  mdata <- mdata[-i]
}