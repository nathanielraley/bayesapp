function(input, output) {
  
  v <- reactiveValues(
    dat = data.frame(id=1:8, expand.grid(Grass=c(0,1), Raining=c(0,1), Sprinkler=c(0,1)))
  )
  
  observeEvent(input$drawS, {
    N = input$N
    newd <- data.frame(id=(nrow(v$dat)+1):(nrow(v$dat)+N), Grass=as.numeric(input$Grass), Raining=as.numeric(input$Raining), Sprinkler=as.numeric(input$Sprinkler))
    v$dat <- rbind(v$dat, newd)
  })
  
  observeEvent(input$reset, {
    v$dat <- data.frame(id=1:8, expand.grid(Grass=c(0,1), Raining=c(0,1), Sprinkler=c(0,1)))
  })
  
  # top plot
  output$plot1 <- renderPlot({
    un <- c("Yes", "No")
    nob <- nrow(v$dat)
    ggplot(data.frame(un, pro=c(sum(v$dat$Raining)/nob, 1-sum(v$dat$Raining)/nob)), aes(x=un,y=pro,fill=un)) + geom_bar(stat="identity") + ylim(0,1) + ylab("Probability") + xlab("Raining?") + 
      theme(strip.text.y=element_text(angle=90), legend.position="none")  + scale_fill_manual(values=c("Yes"="red", "No"="blue"))
  })
  
  # draw sample
  output$plot2 <- renderPlot({
    un <- c("Raining", "Raining", "Not Raining", "Not Raining")
    uno <- c("Yes", "No", "Yes", "No")
    nob1 <- sum(v$dat$Raining==1)
    nob2 <- sum(v$dat$Raining==0)
    pro <- c( sum(v$dat$Sprinkler==1 & v$dat$Raining==1)/nob1, sum(v$dat$Sprinkler==0 & v$dat$Raining==1)/nob1, sum(v$dat$Sprinkler==1 & v$dat$Raining==0)/nob2, sum(v$dat$Sprinkler==0 & v$dat$Raining==0)/nob2 )
    nob <- nrow(v$dat)
    ggplot(data.frame(un, uno, pro=pro), aes(x=uno,y=pro,fill=uno)) + geom_bar(stat="identity") + ylim(0,1) + ylab("Probability") + xlab("Sprinkler On?") + facet_grid(~un) +
      theme(strip.text.y=element_text(angle=90), legend.position="none")  + scale_fill_manual(values=c("Yes"="red", "No"="blue"))
  })
  
  # calculate std difference in means
  output$plot3 <- renderPlot({
    un <- rep(c("Raining", "Raining", "Not Raining", "Not Raining"), each=2)
    uno <- rep(c("Sprinkler On", "Sprinkler On", "Sprinkler Off", "Sprinkler Off"), 2)
    une <- rep(c("Yes", "No"), 4)
    nob1 <- sum(v$dat$Raining==1 & v$dat$Sprinkler==1)
    nob2 <- sum(v$dat$Raining==1 & v$dat$Sprinkler==0)
    nob3 <- sum(v$dat$Raining==0 & v$dat$Sprinkler==1)
    nob4 <- sum(v$dat$Raining==0 & v$dat$Sprinkler==0)
    pro <- c(
      sum(v$dat$Grass==1 & v$dat$Sprinkler==1 & v$dat$Raining==1)/nob1, 
      sum(v$dat$Grass==0 & v$dat$Sprinkler==1 & v$dat$Raining==1)/nob1, 
      sum(v$dat$Grass==1 & v$dat$Sprinkler==0 & v$dat$Raining==1)/nob2, 
      sum(v$dat$Grass==0 & v$dat$Sprinkler==0 & v$dat$Raining==1)/nob2, 
      sum(v$dat$Grass==1 & v$dat$Sprinkler==1 & v$dat$Raining==0)/nob3, 
      sum(v$dat$Grass==0 & v$dat$Sprinkler==1 & v$dat$Raining==0)/nob3, 
      sum(v$dat$Grass==1 & v$dat$Sprinkler==0 & v$dat$Raining==0)/nob4, 
      sum(v$dat$Grass==0 & v$dat$Sprinkler==0 & v$dat$Raining==0)/nob4
    )
    nob <- nrow(v$dat)
    ggplot(data.frame(un, uno, une, pro=pro), aes(x=une,y=pro,fill=une)) + geom_bar(stat="identity") + ylim(0,1) + ylab("Probability") + xlab("Grass Wet?") + facet_grid(uno~un) +
      theme(strip.text.y=element_text(angle=90), legend.position="none") + scale_fill_manual(values=c("Yes"="red", "No"="blue"))
  })
  
  output$plot4 <- renderPlot({
    alpha = ""
    Gc <- paste0(rgb(sum(v$dat$Grass)/nrow(v$dat),0,1-sum(v$dat$Grass)/nrow(v$dat)), alpha)
    Rc <- paste0(rgb(sum(v$dat$Raining)/nrow(v$dat),0,1-sum(v$dat$Raining)/nrow(v$dat)), alpha)
    Sc <- paste0(rgb(sum(v$dat$Sprinkler)/nrow(v$dat),0,1-sum(v$dat$Sprinkler)/nrow(v$dat)), alpha)
    symbols(x=c(1,5,9), y=c(1,0,1), circles=c(3,3,3), inches=0.5, axes=F, xlab="", ylab="", xpd=T, xlim=c(0,10), bg=c(Rc,Gc,Sc))
    #arrows(x0=c(1.5, 8.1, 8.5), x=c(4.3, 1.9, 5.7),y0=c(0.8,1,0.8),c(0.2,1,0.2), angle=20)
    Arrows(x0=c(1.8,2.4, 8.2), x=c(3.9, 7.6, 6.1),y0=c(0.8,1,0.8),c(0.2,1,0.2))
    text(x=c(1,5,9), y=c(1,0,1), labels=c("Raining", "Grass", "Sprinkler"), col="white", family="bold")
    
  })
  
  # if grass wet
  
  output$textOut <- renderText({
    if(input$GrassP=="Wet"){
      
      if(input$whichP=="Rain"){
        prR <- sum(v$dat$Raining)/nrow(v$dat)
        prScR <- sum(v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining)
        prScNR <- sum(v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining)
        prGcScR <- sum(v$dat$Grass & v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining & v$dat$Sprinkler)
        prGcScNR <- sum(v$dat$Grass & v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining & v$dat$Sprinkler)
        prGcNScR <- sum(v$dat$Grass & !v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining & !v$dat$Sprinkler)
        prGcNScNR <- sum(v$dat$Grass & !v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining & !v$dat$Sprinkler)
        norm <- (1-prR) * (1-prScNR) * (prGcNScNR) + (1-prR) * (prScNR) * (prGcScNR) + (prR) * (1-prScR) * (prGcNScR) + (prR) * (prScR) * (prGcScR)
        num <- (prR)*(1-prScR)*(prGcNScR) + (prR)*(prScR)*(prGcScR)
        return(paste0("Pr(Raining|Grass=Wet) = ", round(num/norm,2)))
      } else {
        prR <- sum(v$dat$Raining)/nrow(v$dat)
        prScR <- sum(v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining)
        prScNR <- sum(v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining)
        prGcScR <- sum(v$dat$Grass & v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining & v$dat$Sprinkler)
        prGcScNR <- sum(v$dat$Grass & v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining & v$dat$Sprinkler)
        prGcNScR <- sum(v$dat$Grass & !v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining & !v$dat$Sprinkler)
        prGcNScNR <- sum(v$dat$Grass & !v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining & !v$dat$Sprinkler)
        norm <- (1-prR) * (1-prScNR) * (prGcNScNR) + (1-prR) * (prScNR) * (prGcScNR) + (prR) * (1-prScR) * (prGcNScR) + (prR) * (prScR) * (prGcScR)
        num <- (prR)*(prScR)*(prGcScR) + (1-prR)*(prScNR)*(prGcScNR)
        return(paste0("Pr(Sprinkler|Grass=Wet) = ", round(num/norm,2)))
      }
    }
    # if grass dry
    if(input$GrassP=="Dry"){
      
      if(input$whichP=="Rain"){
        
        prR <- sum(v$dat$Raining)/nrow(v$dat)
        prScR <- sum(v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining)
        prScNR <- sum(v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining)
        prGcScR <- sum(v$dat$Grass & v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining & v$dat$Sprinkler)
        prGcScNR <- sum(v$dat$Grass & v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining & v$dat$Sprinkler)
        prGcNScR <- sum(v$dat$Grass & !v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining & !v$dat$Sprinkler)
        prGcNScNR <- sum(v$dat$Grass & !v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining & !v$dat$Sprinkler)
        norm <- (1-prR) * (1-prScNR) * (1-prGcNScNR) + (1-prR) * (prScNR) * (1-prGcScNR) + (prR) * (1-prScR) * (1-prGcNScR) + (prR) * (prScR) * (1-prGcScR)
        num <- (prR)*(1-prScR)*(1-prGcNScR) + (prR)*(prScR)*(1-prGcScR)
        return(paste0("Pr(Raining|Grass=Dry) = ", round(num/norm,2)))
      } else {
        prR <- sum(v$dat$Raining)/nrow(v$dat)
        prScR <- sum(v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining)
        prScNR <- sum(v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining)
        prGcScR <- sum(v$dat$Grass & v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining & v$dat$Sprinkler)
        prGcScNR <- sum(v$dat$Grass & v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining & v$dat$Sprinkler)
        prGcNScR <- sum(v$dat$Grass & !v$dat$Sprinkler & v$dat$Raining)/sum(v$dat$Raining & !v$dat$Sprinkler)
        prGcNScNR <- sum(v$dat$Grass & !v$dat$Sprinkler & !v$dat$Raining)/sum(!v$dat$Raining & !v$dat$Sprinkler)
        norm <- (1-prR) * (1-prScNR) * (1-prGcNScNR) + (1-prR) * (prScNR) * (1-prGcScNR) + (prR) * (1-prScR) * (1-prGcNScR) + (prR) * (prScR) * (1-prGcScR)
        num <- (prR)*(prScR)*(1-prGcScR) + (1-prR)*(prScNR)*(1-prGcScNR)
        return(paste0("Pr(Sprinkler|Grass=Dry) = ", round(num/norm,2)))
        #return("$$\\mathrm{Pr}(\\mathrm{Sprinkler~On}|\\mathrm{Grass~Dry}) = \\frac{\\sum_{R} \\mathrm{Pr}(\\mathrm{Grass~Dry}|\\mathrm{Raining}=R,~\\mathrm{Sprinkler~On})\\mathrm{Pr}(\\mathrm{Sprinkler~On}|\\mathrm{Raining}=R)}{\\mathrm{Pr}(\\mathrm{Grass~Dry})}$$")
        
      }
    }
    
    
    
  })
  
  output$table <- renderTable({
    out <- ddply(v$dat,.(Grass,Raining,Sprinkler),function(x) c("Number Observations"=nrow(x)))
    out[,1:3] <- ifelse(out[,1:3]==1, "Yes", "No")
    out
  })
  
}
