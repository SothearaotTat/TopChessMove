## packages
library(tidyverse)
library(ggtext)
library(patchwork)
library(png)
library(httr)
library(grid)
library(magrittr)
library(readr)
library(ggplot2)
library(purrr)
library(jpeg)
library(ggforce)
library(ggtext)
library(extrafont)
library(gganimate)
library(camcorder)

#set wd
setwd("D:/Study/self/Chess_visualisation")

#read data
chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')
ECO <- read_csv('ECO-Clean.csv')

gg_record(dir = file.path("D:/Study/self/Chess_visualisation", "recording"),
          device = "png", dpi = 300)


#=============Clean============

#remove unwanted variables
chess_cut <- chess %>% select(-c("game_id", "rated", "start_time", "end_time",
                           "turns", "victory_status", "time_increment", "white_id",
                           "white_rating", "black_id", "black_rating"))

colSums(is.na(chess_cut))

#Join ECO moves
Chess_ft_Open <- chess_cut %>% left_join(ECO, by = c('opening_eco' = 'ECO'))


#Top winning opening
Chess_clean <- Chess_ft_Open %>% group_by(opening_eco, winner) %>%
  summarise(Count = n(),
            .groups = 'drop') %>%
  ungroup %>%
  left_join(ECO, by = c('opening_eco' = 'ECO')) %>%
  arrange(-Count)

TotalWins_chess <- Chess_clean %>%
  mutate(
  win_not = case_when(
    winner == 'draw' ~ 'draw',
    TRUE ~ 'win'
  )) %>% group_by(opening_eco,win_not) %>%
  mutate(
    total_win = sum(Count)
  ) %>%
  arrange(-total_win) %>%
  ungroup()

#top 5
  #function to separate piece from move
separate_capitals <- function(x) {
  Piece <- gsub("[^A-Z]", "", x)  # Get capital letters
  Move <- gsub("[A-Z]", "", x)    # Get the rest
  return(data.frame(Piece, Move, stringsAsFactors = FALSE))
}
  #setting x
  letter <- letters[1:8]
  number <- 1:8
  let.name <- setNames(number,letter)

Top5_move <- unique(TotalWins_chess$opening_eco)[1:5]

Top_5 <- TotalWins_chess %>% filter(opening_eco %in% Top5_move) %>%
  filter(winner != "draw")

#split moves to coordinates
Move_set <- Top_5 %>% select(opening_eco, ECO_move) %>%
  distinct(opening_eco, ECO_move) %>%
  separate(col = ECO_move, into = paste0("move_", 1:2),
           sep = "\\s(?=\\d+\\.)", fill = "right") %>%
  mutate(across(starts_with("move_"),
                ~ gsub("^\\d+\\.\\s*","",.))) %>%
  #move 1
  separate(col = move_1, into = c("move1_white", "move1_black"),
           sep = "\\s")%>%
  #move 2
  separate(col = move_2, into = c("move2_white", "move2_black"),
           sep = "\\s") %>%
  pivot_longer(cols = matches("^move"),
               names_to = "move_color",
               values_to = "move") %>%
  filter(!is.na(move)) %>%
  mutate(across(matches("^move$"), ~ separate_capitals(.))) %>%
  unnest(c(everything()), names_sep = "_") %>%
  #Piece
  mutate(across(ends_with("Piece"), ~ case_when(
    grepl("[A-Z]",.) ~ substr(.,1,1),
    TRUE ~ "pawn"
  ))) %>% 
  mutate(piece = case_when(
    move_Piece == "N" ~ "knight",
    move_Piece  == "R" ~ "rook",
    move_Piece  == "K" ~ "king",
    move_Piece  == "Q" ~ "queen",
    move_Piece == "B" ~ "bishop",
    TRUE ~ "pawn")) %>%
  separate(col = move_color,
           into = c("Move_no", "Color"),
           sep = "_") %>%
  mutate(
    x = gsub("[1-9]","",move_Move),
    y = gsub("[a-z]","",move_Move),
    move_no = as.numeric(gsub("[a-z]","", Move_no))
  ) %>%
  mutate(
    move_seq = as.numeric(case_when(
      Color == "white" & move_no == 1 ~ move_no,
      Color == "black" & move_no == 1 ~ move_no+1,
      Color == "white" & move_no == 2 ~ move_no+1,
      Color == "black" & move_no == 2 ~ move_no+2,
      Color == "white" & move_no == 3 ~ move_no+2,
      Color == "black" & move_no == 3 ~ move_no+3)),
    move = move_Move,
    x = as.numeric(let.name[x]),
    y = as.numeric(y)
    ) %>%
  select(opening_eco, move_seq, move_no, Color, piece,x,y)
  
#return the coordinate to Top_5

Top_5 %<>% left_join(Move_set, by = c("opening_eco" = "opening_eco")) %>%
  select(-c(ECO_move, win_not))
  
#=========separating each of the move sets
  
No1 <- Top_5 %>% filter(opening_eco %in% Top5_move[1])
No2 <- Top_5 %>% filter(opening_eco %in% Top5_move[2])
No3 <- Top_5 %>% filter(opening_eco %in% Top5_move[3])
No4 <- Top_5 %>% filter(opening_eco %in% Top5_move[4])
No5 <- Top_5 %>% filter(opening_eco %in% Top5_move[5])




#=============Importing image============

raster_grob <- vector(mode = "list", length = 18)

store_image <- function(img, width, height, i) {
  
  img <- readPNG(img)
  raster_grob[[i]] <<- rasterGrob(img, width = unit(width, "cm"),
                                  height = unit(height, "cm"),
                                  interpolate = TRUE)}

#white_pawn <- 
store_image("white pawn.png",1,1,1)
#white_rook <-
store_image("white rook.png",1,1,2)
#white_knight <- 
store_image("white knight.png",1,1,3)
#white_bishop <- 
store_image("white bishop.png",1,1,4)
#white_queen <- 
store_image("white queen.png",1,1,5)
#white_king <- 
store_image("white king.png",1,1,6)
store_image("white bishop.png",1,1,7)
store_image("white knight.png",1,1,8)
store_image("white rook.png",1,1,9)
#black_pawn <- 
store_image("black pawn.png",1,1,10)
#black_rook <- 
store_image("black rook.png",1,1,11)
#black_knight <- 
store_image("black knight.png",1,1,12)
#black_bishop <- 
store_image("black bishop.png",1,1,13)
#black_queen <- 
store_image("black queen.png",1,1,14)
#black_king <- 
store_image("black king.png",1,1,15)
store_image("black bishop.png",1,1,16)
store_image("black knight.png",1,1,17)
store_image("black rook.png",1,1,18)

piece_mapping <- c(
  white_pawn = 1, white_rook = 2, white_knight = 3, 
  white_bishop = 4, white_queen = 5, white_king = 6,
  black_pawn = 10, black_rook = 11, black_knight = 12,
  black_bishop = 13, black_queen = 14, black_king = 15
)

#=============Creating chess board=============
chess_board <- data.frame(
  x = rep(1:8, each = 8),
  y = rep(1:8, times = 8))

chess_board %<>% mutate( color =
                           case_when( (x+y) %% 2 ==0 ~ "grey92",
                                      TRUE ~ "grey99"
                           )
)


board <- ggplot(chess_board, aes(x = x, y = y, fill = color)) +
  geom_tile(color = "gray") +  # Add tiles with borders
  scale_fill_identity() +      # Use the colors defined in the data frame
  coord_fixed() +              # Keep aspect ratio square
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = 'grey99', color = NA),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) + #,
        #plot.margin = unit(c(2,1,1,1),"cm"))+
  xlim(0,10) + ylim(0,10) +
  scale_y_continuous(breaks = c(1:10),
                     labels = c(c(1:8),"", ""),
                     expand = c(0,0))+
  scale_x_continuous(breaks = c(1:10),
                     labels = c(c(letters[1:8]),"",""),
                     expand = c(0,0))


for (i in 1:8) {
  board <- board + 
    annotation_custom(
    raster_grob[[1]],  #white pawn
    xmin = i , xmax = i,
    ymin = 2 , ymax = 2
  ) +
    annotation_custom(
      raster_grob[[10]], #black pawn
      xmin = i, xmax= i,
      ymin = 7, ymax =7)
  }

for( w in 1:8){
  board <- board +     
    annotation_custom(
      raster_grob[[w+1]], #white pieces
      xmin = w, xmax= w,
      ymin = 1, ymax =1)}+
    
for( b in 1:8){
  board <- board +
      annotation_custom(
        raster_grob[[b+10]], #white pieces
        xmin = b, xmax= b,
        ymin = 8, ymax =8)}

board




#===============plot 1===============
library(grid)
move_piece_No1 <- list()
coord_No1 <- list()


for (i in 1:length(unique(No1$move_seq))) {
  move_piece_No1[[i]] <- No1 %>% filter(move_no == i) %>%
    select(Color,piece) %>%
    distinct(Color,piece) %>%
    mutate(move_piece = paste(Color,piece,sep="_"))%>%
    pull(move_piece)
  
  coord_No1[[i]] <- No1 %>% filter(move_no == i) %>%
    distinct(Color, piece,x,y) %>%
    select(x,y)

no1_plot <- board

for(i in seq_along(move_piece_No1)){

    for(piece in move_piece_No1[[i]]){
    
    index <- piece_mapping[piece]
    
    x_coord <- coord_No1[[i]]$x
    y_coord <- coord_No1[[i]]$y
    
    no1_plot <- no1_plot + annotation_custom(
      raster_grob[[index]],
      xmin = x_coord - 0.5, xmax = x_coord + 0.5,
      ymin = y_coord - 0.5, ymax = y_coord + 0.5)
    }
  }
}



no1_plot <- no1_plot + geom_tile(data = data.frame(x = 2,y=2),
                     aes(x = x, y = y),
                     fill = "grey92") + 
  geom_rect(aes(xmin = 0.5, xmax= 8.5, ymin= 8.52, ymax = 9.1),
            fill = "indianred", color = "grey92", size = 0.5) +
  geom_text(aes(x = 4.5, y = 8.7,
           label = "The Polish Defense"),
           size = 4, vjust = 0, color = 'white',
           family = "Georgia")+
  ylim(0.5,10) + 
  scale_y_continuous(breaks = c(0.5,1:10),
                     labels = c("",1:8,"",""),
                     expand = expansion(mult = c(0,0.1)))
  
no1_plot

#===============plot 2===============
move_piece_No2 <- list()
coord_No2 <- list()

for (i in 1:length(unique(No2$move_seq))) {
  move_piece_No2[[i]] <- No2 %>% filter(move_seq == i) %>%
    select(Color,piece) %>%
    distinct(Color,piece) %>%
    mutate(move_piece = paste(Color,piece,sep="_"))%>%
    pull(move_piece)
  
  coord_No2[[i]] <- No2 %>% filter(move_seq == i) %>%
    distinct(Color, piece,x,y) %>%
    select(x,y)
  
  no2_plot <- board
  
  for(i in seq_along(move_piece_No2)){
    
    for(piece in move_piece_No2[[i]]){

      index_2 <- piece_mapping[piece]
      
      x_coord <- coord_No2[[i]]$x
      y_coord <- coord_No2[[i]]$y
      
      
      no2_plot <- no2_plot + annotation_custom(
        raster_grob[[index_2]],
        xmin = x_coord - 0.5, xmax = x_coord + 0.5,
        ymin = y_coord - 0.5, ymax = y_coord + 0.5)
    }
  }
}

no2_plot <- no2_plot + geom_tile(data = data.frame(x = c(5,5),y=c(2,7),
                                                   fill_color = c("grey99","grey92")),
                                 aes(x = x, y = y, fill = fill_color),
                                 width = 1, height = 1)+
  geom_rect(aes(xmin = 0.5, xmax= 8.5, ymin= 8.52, ymax = 9.1),
            fill = "dodgerblue4", color = "grey92", size = 0.5) +
  geom_text(aes(x = 4.5, y = 8.7,
                label = "The French Defense"),
            size = 4, vjust = 0, color = 'grey92',
            family = "Georgia")+
  ylim(0.5,10) + 
  scale_y_continuous(breaks = c(0.5,1:10),
                     labels = c("",1:8,"",""),
                     expand = expansion(mult = c(0,0.1)))

no2_plot

#===============plot 3===============
move_piece_No3 <- list()
coord_No3 <- list()

for (i in 1:length(unique(No3$move_seq))) {
  move_piece_No3[[i]] <- No3 %>% filter(move_seq == i) %>%
    select(Color,piece) %>%
    distinct(Color,piece) %>%
    mutate(move_piece = paste(Color,piece,sep="_"))%>%
    pull(move_piece)
  
  coord_No3[[i]] <- No3 %>% filter(move_seq == i) %>%
    distinct(Color, piece,x,y) %>%
    select(x,y)
  
  no3_plot <- board
  
  for(i in seq_along(move_piece_No3)){
    
    for(piece in move_piece_No3[[i]]){
      
      index_3 <- piece_mapping[piece]
      
      x_coord <- coord_No3[[i]]$x
      y_coord <- coord_No3[[i]]$y
      
      
      no3_plot <- no3_plot + annotation_custom(
        raster_grob[[index_3]],
        xmin = x_coord - 0.5, xmax = x_coord + 0.5,
        ymin = y_coord - 0.5, ymax = y_coord + 0.5)
    }
  }
}

no3_plot <- no3_plot + geom_tile(data = data.frame(x = c(4,4),y=c(2,7),
                                                   fill_color = c("grey92","grey99")),
                                 aes(x = x, y = y, fill = fill_color),
                                 width = 1, height = 1)+
  geom_rect(aes(xmin = 0.5, xmax= 8.5, ymin= 8.52, ymax = 9.1),
            fill = "#FFCE5C", color = "grey92", size = 0.5) +
  geom_text(aes(x = 4.5, y = 8.7,
                label = "The Queen's Pawn Game"),
            size = 4, vjust = 0, color = 'grey10',
            family = "Georgia")+
  ylim(0.5,10) + 
  scale_y_continuous(breaks = c(0.5,1:10),
                     labels = c("",1:8,"",""),
                     expand = expansion(mult = c(0,0.1)))

no3_plot

#===============plot 4===============
move_piece_No4 <- list()
coord_No4 <- list()

for (i in 1:length(unique(No4$move_seq))) {
  move_piece_No4[[i]] <- No4 %>% filter(move_seq == i) %>%
    select(Color,piece) %>%
    distinct(Color,piece) %>%
    mutate(move_piece = paste(Color,piece,sep="_"))%>%
    pull(move_piece)
  
  coord_No4[[i]] <- No4 %>% filter(move_seq == i) %>%
    distinct(Color, piece,x,y) %>%
    select(x,y)
  
  no4_plot <- board
  
  for(i in seq_along(move_piece_No4)){
    
    for(piece in move_piece_No4[[i]]){
      
      index_4 <- piece_mapping[piece]
      
      x_coord <- coord_No4[[i]]$x
      y_coord <- coord_No4[[i]]$y
      
      
      no4_plot <- no4_plot + annotation_custom(
        raster_grob[[index_4]],
        xmin = x_coord - 0.5, xmax = x_coord + 0.5,
        ymin = y_coord - 0.5, ymax = y_coord + 0.5)
    }
  }
}


no4_plot <- no4_plot + geom_tile(data = data.frame(x = c(5,4),y=c(2,7),
                                                   fill_color = c("grey99","grey99")),
                                 aes(x = x, y = y, fill = fill_color),
                                 width = 1, height = 1)+
  geom_rect(aes(xmin = 0.5, xmax= 8.5, ymin= 8.52, ymax = 9.1),
            fill = "#91A776", color = "grey92", size = 0.5) +
  geom_text(aes(x = 4.5, y = 8.7,
                label = "The Scandinavian Defense"),
            size = 4, vjust = 0, color = 'grey92',
            family = "Georgia")+
  ylim(0.5,10) + 
  scale_y_continuous(breaks = c(0.5,1:10),
                     labels = c("",1:8,"",""),
                     expand = expansion(mult = c(0,0.1)))

no4_plot

#===============plot 5===============
move_piece_No5 <- list()
coord_No5 <- list()

for (i in 1:length(unique(No5$move_seq))) {
  move_piece_No5[[i]] <- No5 %>% filter(move_seq == i) %>%
    select(Color,piece) %>%
    distinct(Color,piece) %>%
    mutate(move_piece = paste(Color,piece,sep="_"))%>%
    pull(move_piece)
  
  
  coord_No5[[i]] <- No5 %>% filter(move_seq == i) %>%
    distinct(Color, piece,x,y) %>%
    select(x,y)
  
  no5_plot <- board
  
  for(i in seq_along(move_piece_No5)){
    
    for(piece in move_piece_No5[[i]]){
      
      index_5 <- piece_mapping[piece]
      
      x_coord <- coord_No5[[i]]$x
      y_coord <- coord_No5[[i]]$y
      
      
      no5_plot <- no5_plot + annotation_custom(
        raster_grob[[index_5]],
        xmin = x_coord - 0.5, xmax = x_coord + 0.5,
        ymin = y_coord - 0.5, ymax = y_coord + 0.5)
    }
  }
}

no5_plot <- no5_plot + geom_tile(data = data.frame(x = c(5,7,4,5),y=c(2,1,7,7),
                                                   fill_color = c("grey99","grey92",
                                                                  "grey99", "grey92")),
                                 aes(x = x, y = y, fill = fill_color),
                                 width = 1, height = 1) +
  geom_arc(aes(x0= 4.5, y0 = 4,
               r = 0.8, start = pi/2, end = pi/4),
           color = "indianred",
           linewidth = 1,
           arrow = arrow(type = "open", length = unit(0.1,"inches")),
           inherit.aes = FALSE)+
  geom_arc(aes(x0= 3.1, y0 = 3.1,
               r = 3, start = pi/4, end = pi*17/36),
           color = "indianred",
           linewidth = 1,
           arrow = arrow(type = "open", length = unit(0.1,"inches")),
           inherit.aes = FALSE)+
  geom_arc(aes(x0= 7, y0 = 5.5,
               r = 3, start = pi*7/6, end = pi*3/2),
           color = "indianred",
           linewidth = 1,
           arrow = arrow(type = "open", length = unit(0.1,"inches")),
           inherit.aes = FALSE)+
  
  geom_rect(aes(xmin = 0.5, xmax= 8.5, ymin= 8.52, ymax = 9.1),
            fill = "#468186", color = "grey92", size = 0.5) +
  geom_text(aes(x = 4.5, y = 8.7,
                label = "The Philidor's Defense"),
            size = 4, vjust = 0, color = 'grey90',
            family = "Georgia")+
  ylim(0.5,10) + 
  scale_y_continuous(breaks = c(0.5,1:10),
                     labels = c("",1:8,"",""),
                     expand = expansion(mult = c(0,0.1)))
no5_plot


#==========description=========
library(showtext)
library(extrafont)
font_import(pattern = "Lato")


loadfonts(device = "win")

title_img <- readPNG("Title.png")
raster_title <- rasterGrob(title_img, width = unit(10,"cm"),
                           height = unit(5,"cm"))

Desc <- data.frame(x =0:10, y = 0:10)
  
  Desc_plot <- Desc %>%
    ggplot(aes(x = x, y = y)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = 'grey99', color = NA)) +
    scale_y_continuous(limits = c(0,10),
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0,10),
                       expand = c(0, 0)) +
    geom_text(aes(x = 0, y = 3,  # Specify the y value for the text
                  label = "The Grand Openings"),
              hjust = 0,
              family = "Georgia", fontface = 'bold',
              size = 6.5)+
    geom_text(aes(x = 0, y = 1.25,
                      label = "The following are top chess openings\nfrom over 20,000 games on Lichess.\nThe arrows show the sequence of moves.\nWhite to move first!"),
                  size = 3.5, family = "Lato", hjust = 0,
              color = 'grey50') + 
    annotation_custom(raster_title,
                      xmin = 5, xmax = 5,
                      ymin = 6.5, ymax = 6.5)
  
  
  
  Desc_plot
#==================caption==============
caption <- 
    ggplot(data.frame(x = 1:2, y = 1:10)) +
    labs(x = NULL, y = NULL,
         caption = "Visualization by Sothearaot Tat  |  Source: Lichess&Kaggle/Mitchell J  |  Image by: Freepik(title) & Wikimedian(piece)") +
    theme(line = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", 
                                         color = "transparent"),
          panel.border = element_rect(color = "transparent"),
          axis.text = element_blank())
  
caption
#============THE PLOOOOOOOOOOOT=================

  
Final_plot <- Desc_plot + no1_plot + no2_plot + no3_plot +  no4_plot + no5_plot

Final_plot / caption + plot_layout(nrow = 2, heights = c(5,0))


ggsave("The Grand Opening-2024-09.png",width = 12, height = 9)


gg_resize_film(width = 12, height = 9, dpi = 300)
gg_stop_recording()
gg_playback(first_image_duration = 1,
            last_image_duration = 15,
            frame_duration = 0.5,
            image_resize = 800)




