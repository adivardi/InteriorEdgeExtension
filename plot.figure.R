plot.figure <- function(df){
  filters <- df$t %>% unique() %>% as.character()
  v <- lapply(filters, function(filter){
    # new_df <- df %>% filter(t == m.filter)
    new.df <- df[df$t == filter,]%>% rbind(df[df$t== filter,][1,])  # append a raw with the 1st vertex again to close the polygon
    if (filter == filters[1]) {
      plot(new.df[,1:2], type = 'l', lwd = 2, col = 'blue')
    }else{
      lines(new.df[,1:2], type = 'l', lwd = 2, col = 'red')
      obs.x <- new.df[,1] %>% as.matrix() %>% as.numeric()
      obs.y <- new.df[,2] %>% as.matrix() %>% as.numeric()
      polygon(obs.x, obs.y, col = "grey70", border = NA)
    }
  })
}

lines.figure <- function(df){
  filters <- df$t %>% unique() %>% as.character()
  v <- lapply(filters, function(filter){
    # new_df <- df %>% filter(t == m.filter)
    new.df <- df[df$t == filter,]%>% rbind(df[df$t== filter,][1,])
    obs.x <- new.df[,1] %>% as.matrix() %>% as.numeric()
    obs.y <- new.df[,2] %>% as.matrix() %>% as.numeric()
    polygon(obs.x, obs.y, col = "chocolate", border = NA)
    lines(new.df[,1:2], type = 'l', lwd = 2, col = 'black')
  })
}

plot.figure.subpoly <- function(subpolygon_list){
  library(RColorBrewer)
  # p <- palette(rainbow(length(subpolygon_list)))
  palette <- brewer.pal(11, "Spectral")
  palette <- colorRampPalette(palette)(length(subpolygon_list))  # expand palette
  for (i in 1:length(subpolygon_list)){

    subpolygon <- subpolygon_list[[i]]  # df
    subpolygon <- subpolygon %>% rbind(subpolygon[1,]) # append a raw with the 1st vertex again to close the polygon
    print(subpolygon[,1:2])
    # plot boundaries
    lines(subpolygon[,1:2], type = 'l', lwd = 4, col = palette[i])#'blueviolet')  # add to plot

    # plot polygons
    # obs.x <- new.df[,1] %>% as.matrix() %>% as.numeric()
    # obs.y <- new.df[,2] %>% as.matrix() %>% as.numeric()
    # polygon(obs.x, obs.y, col = addalpha(palette[i], 0.05), border = NA)
    # rgb(0.6, 0, 0.6, 0.05)
  }
}

addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}
