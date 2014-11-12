ggplotHelper <- function(p, save=NULL, title=NULL, size=NULL)
{
  if (! is.null(title)){
    p + ggtitle(title)
  }
  if (! is.null(save)){
    if (! is.null(size)){
      p + ggsave(save, width=size[1], height=size[2])      
    }
    else{
      p + ggsave(save)
    }
  }
  else{
    p
  } 
}

SingleDensity <- function(data1, x.string, save=NULL, title=NULL, size=NULL){
  p <- ggplot() + 
    geom_density(data=data1, aes_string(x=x.string, 
                                        group='factor(label)', 
                                        fill='factor(label)'), alpha=0.5)
  ggplotHelper(p, save, title, size)
}

DoubleDensity <- function(data1, data2, x.string, save=NULL, title=NULL, size=NULL){
  p <- ggplot() + 
    geom_density(data=data1, aes_string(x=x.string, group='factor(label)', fill='factor(label)'), alpha=0.5) +
    geom_density(data=data2, aes_string(x=x.string), fill='#000000', alpha=0.5)
  ggplotHelper(p, save, title, size)
}

SingleScatter <- function(data1, x.string, y.string, save=NULL, title=NULL, size=NULL){
  p <- ggplot() + 
    geom_point(data=data1, aes_string(x=x.string, y=y.string, color='factor(label)'), alpha=.5, size=1)
  ggplotHelper(p, save, title, size)
}

DoubleScatter <- function(data1, data2, x.string, y.string, save=NULL, title=NULL, size=NULL){
  p <- ggplot() + 
    geom_point(data=data1, aes_string(x=x.string, y=y.string, color='factor(label)'), alpha=.5, size=1) + 
    geom_point(data=data2, aes_string(x=x.string, y=y.string), alpha=.5, size=1, color='black')  
  ggplotHelper(p, save, title, size)
}
