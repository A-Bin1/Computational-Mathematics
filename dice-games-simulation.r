#Dice Game Simulation Using R (Introduction to Probability Theory)

#set seed for producing same numbers
set.seed(1)

#create individual functions for each game with parameter n plays
#Game 1: roll a die one time, WIN if you have an Ace (a ‘6’).
game1func <- function(n) {
    return(rbinom(n, 1, 1/6))
}

#Game 2: roll a die four times, WIN if you have one or more Aces.
game2func <- function(n){
    return(rbinom(n, 4, 1/6))
}

#Game 3: roll a pair of dice 24 times, WIN if you have one or more Double Aces.
game3func <- function(n){
    return(rbinom(n, 24, 1/36))
}


#creating function to combine all games into a single dataframe for organizational purposes
gfunc <- function(){
    plays_numlst <- c(100,200,400,500,800,1000)
    wins_lst1 <- list()
    wins_lst2 <- list()
    wins_lst3 <- list()
    loss_lst <- list()
    game_num_lst <- list()

    
    #Each for loop is a different game.
    for(i in plays_numlst){
        x <- game1func(i)
        wins_lst1 <- c(wins_lst1,sum(x==1))
        loss_lst <- c(loss_lst,sum(x==0))
        game_num_lst <- c(game_num_lst,'Game1')
        cum_Wins_lst1 <- c(ave(wins_lst1, game_num_lst, FUN = cumsum))
        cum_Plays_lst1 <- c(ave(plays_numlst, game_num_lst, FUN = cumsum))
        }

    for(i in plays_numlst){
        x <- game2func(i)
        wins_lst2 <- c(wins_lst2,sum(x>0))
        loss_lst <- c(loss_lst,sum(x==0))
        game_num_lst <- c(game_num_lst,'Game2')
        cum_Wins_lst2 <- c(ave(wins_lst2, game_num_lst, FUN = cumsum))
        cum_Plays_lst2 <- c(ave(plays_numlst, game_num_lst, FUN = cumsum))
        }

    for(i in plays_numlst){
        x <- game3func(i)
        wins_lst3 <- c(wins_lst3,sum(x>0))
        loss_lst <- c(loss_lst,sum(x==0))
        game_num_lst <- c(game_num_lst,'Game3')
        cum_Wins_lst3 <- c(ave(wins_lst3, game_num_lst, FUN = cumsum))
        cum_Plays_lst3 <- c(ave(plays_numlst, game_num_lst, FUN = cumsum))
        }

        wins_lst <- c(wins_lst1, wins_lst2, wins_lst3)
        cum_Wins_lst <- c(cum_Wins_lst1, cum_Wins_lst2, cum_Wins_lst3)
        cum_Plays_lst <- c(cum_Plays_lst1,cum_Plays_lst2,cum_Plays_lst3)

       
        wdf <- t(data.frame(wins_lst))
        gdf <- t(data.frame(game_num_lst))
        ldf <- t(data.frame(loss_lst))
        cw_df <- t(data.frame(cum_Wins_lst))
        cp_df <- data.frame(cum_Plays_lst)

        df <- cbind(gdf,plays_numlst, ldf, wdf,cp_df, cw_df)
        colnames(df) <- c("Game", "Plays",  "Losses","Wins", "Cumulative_Plays", "Cumulative_Wins")
        df$Relat_Freq <- df$Wins/df$Plays #add relative frequency column after df is created
        return(df) #df will show a unique ID along with the columns
}

print(gfunc())
