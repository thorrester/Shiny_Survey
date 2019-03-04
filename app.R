library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)
library(corrplot)


### Load the dataset
df <- read.csv("Data/df.csv")
cdf <- read.csv("Data/new_df.csv")



ui <- fluidPage(
  
  # Title
    titlePanel("Emory Engagement Survey"),
    
    fluidRow(
    
    column(2,
           wellPanel(
               selectInput("dept", "Department", choices = levels(unique(df$Department)), selected = "SOM")
           )
         ),
    
    mainPanel(h3(textOutput("selection"))),
    
    
    column(5,
           plotOutput("plot1"),
           plotOutput("plot2"),
           plotOutput("plot3"),
           plotOutput("plot4")
         ),
    column(5,
           plotOutput("plot5"),
           plotOutput("plot6"),
           plotOutput("plot7")
    )
  )
)


server <- function(input, output) {
  
######## Slected Title
  
  output$selection <- renderText({
    sprintf('Survey Results For %s', input$dept)
  })
########################################################### Plot 1
  
  output$plot1 <- renderPlot({
    
    #Filter Building Trust
    ks <-
      df %>%
      filter(Department == input$dept & question == "Building Trust"
      )
    
    ks2 <-setNames(aggregate(ks$prop, list(Group = ks$group), sum), c("Group", "prop"))
    
    ksp <- ggplot(ks, aes( x = score, y = prop, fill = score)) + 
      geom_bar(stat = "identity", color = "black", fill = "#85C1E9") +
      scale_x_continuous(name="", breaks = c(0, 1, 2, 3, 4, 5)) +
      scale_y_continuous(name= "Proportion") +
      theme(axis.text.x = element_text(color="Black", size=12),
            axis.text.y = element_text(color="Black", size=12), 
            axis.title = element_text(face = "bold", color = "Black", size = 12),
            plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    ksgroup <- ggplot(ks2, aes( x = Group, y = prop, fill = Group)) + 
      geom_bar(stat = "identity", fill = "#85C1E9", color = "black") +
      scale_x_discrete(name="Score") +
      scale_y_continuous(name= "") +
      theme(axis.text.x = element_text(color="Black", size=12),
            axis.text.y = element_text(color="Black", size=12), 
            axis.title = element_text(face = "bold", color = "Black", size = 12),
            plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
      geom_hline(yintercept = 40, linetype = "dashed", size = .75) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      coord_flip()
    
    k <- grid.arrange(ksp, ksgroup, ncol = 2, 
                      top = textGrob("Building Trust",
                                     gp=gpar(fontface = "bold", fontsize = 18)))
    
    #Filter Collaboration
    s <-
      df %>%
      filter(Department == input$dept & question == "Collaboration"
      ) 
    
    s2 <-setNames(aggregate(s$prop, list(Group = s$group), sum), c("Group", "prop"))
    
    
    sp <- ggplot(s, aes( x = score, y = prop, fill = score)) + 
      geom_bar(stat = "identity", color = "black", fill = "#BFC0C1") +
      scale_x_continuous(name="", breaks = c(0, 1, 2, 3, 4, 5)) +
      scale_y_continuous(name= "Proportion") +
      theme(axis.text.x = element_text(color="Black", size=12),
            axis.text.y = element_text(color="Black", size=12), 
            axis.title = element_text(face = "bold", color = "Black", size = 12),
            plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    sgroup <- ggplot(s2, aes( x = Group, y = prop, fill = Group)) + 
      geom_bar(stat = "identity", fill = "#BFC0C1", color = "black") +
      scale_x_discrete(name="Score") +
      scale_y_continuous(name= "") +
      theme(axis.text.x = element_text(color="Black", size=12),
            axis.text.y = element_text(color="Black", size=12), 
            axis.title = element_text(face = "bold", color = "Black", size = 12),
            plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
      geom_hline(yintercept = 40, linetype = "dashed", size = .75) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      coord_flip()
    
    s <- grid.arrange(sp,sgroup, ncol = 2,
                      top = textGrob("Collaboration",
                                     gp=gpar(fontface = "bold", fontsize = 18)))
    
    
    grid.arrange(k, s, nrow = 2)       
    
    
  })
  

########################################################  PLot 2

output$plot2 <- renderPlot({
    
    #Filter Communication
    ks <-
      df %>%
      filter(Department == input$dept & question == "Communication"
      )
    
    ks2 <-setNames(aggregate(ks$prop, list(Group = ks$group), sum), c("Group", "prop"))
    
    ksp <- ggplot(ks, aes( x = score, y = prop, fill = score)) + 
      geom_bar(stat = "identity", color = "black", fill = "#85C1E9") +
      scale_x_continuous(name="", breaks = c(0, 1, 2, 3, 4, 5)) +
      scale_y_continuous(name= "Proportion") +
      theme(axis.text.x = element_text(color="Black", size=12),
            axis.text.y = element_text(color="Black", size=12), 
            axis.title = element_text(face = "bold", color = "Black", size = 12),
            plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    ksgroup <- ggplot(ks2, aes( x = Group, y = prop, fill = Group)) + 
      geom_bar(stat = "identity", fill = "#85C1E9", color = "black") +
      scale_x_discrete(name="Score") +
      scale_y_continuous(name= "") +
      theme(axis.text.x = element_text(color="Black", size=12),
            axis.text.y = element_text(color="Black", size=12), 
            axis.title = element_text(face = "bold", color = "Black", size = 12),
            plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
      geom_hline(yintercept = 40, linetype = "dashed", size = .75) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      coord_flip()
    
    k <- grid.arrange(ksp, ksgroup, ncol = 2, 
                      top = textGrob("Communication",
                                  gp=gpar(fontface = "bold", fontsize = 18)))
    
    #Filter delivering results
    s <-
      df %>%
      filter(Department == input$dept & question == "Delivering Results"
      )
    s2 <-setNames(aggregate(s$prop, list(Group = s$group), sum), c("Group", "prop"))
    
    
    sp <- ggplot(s, aes( x = score, y = prop, fill = score)) + 
      geom_bar(stat = "identity", color = "black", fill = "#BFC0C1") +
      scale_x_continuous(name="", breaks = c(0, 1, 2, 3, 4, 5)) +
      scale_y_continuous(name= "Proportion") +
      theme(axis.text.x = element_text(color="Black", size=12),
            axis.text.y = element_text(color="Black", size=12), 
            axis.title = element_text(face = "bold", color = "Black", size = 12),
            plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    sgroup <- ggplot(s2, aes( x = Group, y = prop, fill = Group)) + 
      geom_bar(stat = "identity", fill = "#BFC0C1", color = "black") +
      scale_x_discrete(name="Score") +
      scale_y_continuous(name= "") +
      theme(axis.text.x = element_text(color="Black", size=12),
            axis.text.y = element_text(color="Black", size=12), 
            axis.title = element_text(face = "bold", color = "Black", size = 12),
            plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
      geom_hline(yintercept = 40, linetype = "dashed", size = .75) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      coord_flip()
    
    s <- grid.arrange(sp,sgroup, ncol = 2,
                      top = textGrob("Delivering Results",
                                    gp=gpar(fontface = "bold", fontsize = 18)))
    
    
    grid.arrange(k, s, nrow = 2)       
    
    
  })
  
###################################################### PLot 3
output$plot3 <- renderPlot({
  
  #Filter Knowledge Skills
  ks <-
    df %>%
    filter(Department == input$dept & question == "Knowledge Skills"
    )
  
  ks2 <-setNames(aggregate(ks$prop, list(Group = ks$group), sum), c("Group", "prop"))
  
  ksp <- ggplot(ks, aes( x = score, y = prop, fill = score)) + 
    geom_bar(stat = "identity", color = "black", fill = "#85C1E9") +
    scale_x_continuous(name="", breaks = c(0, 1, 2, 3, 4, 5)) +
    scale_y_continuous(name= "Proportion") +
    theme(axis.text.x = element_text(color="Black", size=12),
          axis.text.y = element_text(color="Black", size=12), 
          axis.title = element_text(face = "bold", color = "Black", size = 12),
          plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  ksgroup <- ggplot(ks2, aes( x = Group, y = prop, fill = Group)) + 
    geom_bar(stat = "identity", fill = "#85C1E9", color = "black") +
    scale_x_discrete(name="Score") +
    scale_y_continuous(name= "") +
    theme(axis.text.x = element_text(color="Black", size=12),
          axis.text.y = element_text(color="Black", size=12), 
          axis.title = element_text(face = "bold", color = "Black", size = 12),
          plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
    geom_hline(yintercept = 40, linetype = "dashed", size = .75) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    coord_flip()
  
  
  k <- grid.arrange(ksp, ksgroup, ncol = 2, 
                    top = textGrob("Knowledge Skills",
                                   gp=gpar(fontface = "bold", fontsize = 18)))
  
  #Filter Problem Solving
  s <-
    df %>%
    filter(Department == input$dept & question == "Problem Solving"
    )
  s2 <-setNames(aggregate(s$prop, list(Group = s$group), sum), c("Group", "prop"))
  
  
  sp <- ggplot(s, aes( x = score, y = prop, fill = score)) + 
    geom_bar(stat = "identity", color = "black", fill = "#BFC0C1") +
    scale_x_continuous(name="", breaks = c(0, 1, 2, 3, 4, 5)) +
    scale_y_continuous(name= "Proportion") +
    theme(axis.text.x = element_text(color="Black", size=12),
          axis.text.y = element_text(color="Black", size=12), 
          axis.title = element_text(face = "bold", color = "Black", size = 12),
          plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  sgroup <- ggplot(s2, aes( x = Group, y = prop, fill = Group)) + 
    geom_bar(stat = "identity", fill = "#BFC0C1", color = "black") +
    scale_x_discrete(name="Score") +
    scale_y_continuous(name= "") +
    theme(axis.text.x = element_text(color="Black", size=12),
          axis.text.y = element_text(color="Black", size=12), 
          axis.title = element_text(face = "bold", color = "Black", size = 12),
          plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
    geom_hline(yintercept = 40, linetype = "dashed", size = .75) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    coord_flip()
  
  s <- grid.arrange(sp,sgroup, ncol = 2,
                    top = textGrob("Problem Solving",
                                   gp=gpar(fontface = "bold", fontsize = 18)))
  
  
  grid.arrange(k, s, nrow = 2)       
  
  
})

##################################################################Plot 4
output$plot4 <- renderPlot({
  
  #Filter Service
  ks <-
    df %>%
    filter(Department == input$dept & question == "Service"
    )
  
  ks2 <-setNames(aggregate(ks$prop, list(Group = ks$group), sum), c("Group", "prop"))
  
  ksp <- ggplot(ks, aes( x = score, y = prop, fill = score)) + 
    geom_bar(stat = "identity", color = "black", fill = "#85C1E9") +
    scale_x_continuous(name="", breaks = c(0, 1, 2, 3, 4, 5)) +
    scale_y_continuous(name= "Proportion") +
    theme(axis.text.x = element_text(color="Black", size=12),
          axis.text.y = element_text(color="Black", size=12), 
          axis.title = element_text(face = "bold", color = "Black", size = 12),
          plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  ksgroup <- ggplot(ks2, aes( x = Group, y = prop, fill = Group)) + 
    geom_bar(stat = "identity", fill = "#85C1E9", color = "black") +
    scale_x_discrete(name="Score") +
    scale_y_continuous(name= "") +
    theme(axis.text.x = element_text(color="Black", size=12),
          axis.text.y = element_text(color="Black", size=12), 
          axis.title = element_text(face = "bold", color = "Black", size = 12),
          plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
    geom_hline(yintercept = 40, linetype = "dashed", size = .75) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    coord_flip()
  
  
  k <- grid.arrange(ksp, ksgroup, ncol = 2, 
                    top = textGrob("Service",
                                   gp=gpar(fontface = "bold", fontsize = 18)))
  
  #Filter Taking Initiative
  s <-
    df %>%
    filter(Department == input$dept & question == "Taking Initiative"
    )
  s2 <-setNames(aggregate(s$prop, list(Group = s$group), sum), c("Group", "prop"))
  
  
  sp <- ggplot(s, aes( x = score, y = prop, fill = score)) + 
    geom_bar(stat = "identity", color = "black", fill = "#BFC0C1") +
    scale_x_continuous(name="Score", breaks = c(0, 1, 2, 3, 4, 5)) +
    scale_y_continuous(name= "Proportion") +
    theme(axis.text.x = element_text(color="Black", size=12),
          axis.text.y = element_text(color="Black", size=12), 
          axis.title = element_text(face = "bold", color = "Black", size = 12),
          plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  sgroup <- ggplot(s2, aes( x = Group, y = prop, fill = Group)) + 
    geom_bar(stat = "identity", fill = "#BFC0C1", color = "black") +
    scale_x_discrete(name="Score") +
    scale_y_continuous(name= "Proportion") +
    theme(axis.text.x = element_text(color="Black", size=12),
          axis.text.y = element_text(color="Black", size=12), 
          axis.title = element_text(face = "bold", color = "Black", size = 12),
          plot.title = element_text(color="Black", size=12, face="bold", hjust = 0.5)) +
    geom_hline(yintercept = 40, linetype = "dashed", size = .75) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    coord_flip()
  
  s <- grid.arrange(sp,sgroup, ncol = 2,
                    top = textGrob("Taking Initiative",
                                   gp=gpar(fontface = "bold", fontsize = 18)))
  
  
  grid.arrange(k, s, nrow = 2)       
  
  
  
})
############################################ PLot 5
output$plot5 <- renderPlot({
  
  #Filter Service
  avg <-
   cdf %>%
    filter(Department == input$dept)
  
  avg1 <- select(avg, Average.Rating)
    
  p <- ggdensity(avg1, x = "Average.Rating", color = "black",
                   fill = "#85C1E9", 
                   alpha = 0.3, 
                   add = c("mean"),
                   ylab = "Density",
                   xlab = "Score",
                   font.label = list(size = 12, color = "black"))
  avgp <- p +
              font("xlab", size = 12, face = "bold") +
              font("ylab", size = 12, face = "bold")
                   
    
    
  
  grid.arrange(avgp, ncol = 1,
               top = textGrob("Density Plot of Average Score (Dashed Line = Mean)",
                            gp=gpar(fontface = "bold", fontsize = 18)))
        
  
  
})
######################### PLot 6
output$plot6 <- renderPlot({
  
  #Corrplot
  cordf <-
    cdf %>%
    filter(Department == input$dept) %>% 
    select("Average.Rating", 
           "Building.Trust", 
           "Collaboration", 
           "Communication", 
           "Delivering.Results", 
           "Knowledge.Skills", 
           "Problem.Solving", 
           "Service", 
           "Taking.Initiative")
  
  corrplot(cor(cordf), method = "number", number.cex = 1, tl.cex = 1)
  
},  height = 750, width = 750)

####################End 
}
shinyApp(ui = ui, server = server)
