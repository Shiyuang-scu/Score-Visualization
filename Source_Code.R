get_filename <- function(){
        number <- "17311010"
        filename <- c()
        for(i in sequence(10)){
                aft <- paste(number,i,sep="")
                filename <- c(filename, paste(aft,".xlsx",sep = ""))
        }
        filename <- filename[1:9]
        filename <- c(filename,"173110110.xlsx")
        filename
}

get_name <- function(filename){
        name <- c()
        for(file in filename){
                data <- read.xlsx(file)
                data <- data[c(5:nrow(data)),]
                data_name <- data$X3
                name <- union(name, data_name)
        }
        name
}

get_class <- function(filename){
        Class <- c()
        popul <- c()
        for(file in filename){
                data <- read.xlsx(file)
                data <- data[c(5:nrow(data)),]
                popul <- c(popul,nrow(data))
        }
        for(i in c(1:10)){
                Class <- c(Class,rep(i,popul[i]))
        }
        Class
}

get_scores <- function(filename){
        scores <- c()
        for(file in filename){
                data <- read.xlsx(file)
                data <- data[c(5:nrow(data)),]
                data_scores <- data[-1:(-ncol(data)+3)]
                data_scores <- data_scores[-2:-3]
                data_scores <- data_scores[,1]
                scores <- c(scores, data_scores)
        }
        scores
}

Class_Scores_bar<-function(){
        ggplot(class_scores,aes(x=sequence(10),y=Ave_Scores,fill=Class,group=factor(3)))+
                geom_bar(stat = "identity")+theme_tufte() +
                scale_x_continuous(breaks = class_scores$Class) +
                geom_text(aes(label = class_scores$Ave_Scores, vjust = -0.8, hjust = 0.5, color = Class), show.legend = FALSE) +
                ylim(min(class_scores$Ave_Scores, 0)*1.1, max(class_scores$Ave_Scores)*1.1) +
                labs(x="Class",y="Average Scores") +
                theme(axis.title.x = element_text(size=20,face = "bold"),axis.title.y = element_text(size=20,face="bold")) +
                labs(title = "The Distribution of Average Scores in 10 Classes") +
                theme(plot.title = element_text(hjust = 0.5,face = "bold", size = 20))
        }


Class_Scores_scatter<-function(){
        ggplot(data,aes(x = as.numeric(Class) ,y= as.numeric(as.character(Scores)), colour = Class)) + 
                geom_point(shape=19, position = position_dodge(0.1)) + 
                xlab("Class") + 
                ylab("Scores") +
                scale_x_continuous(breaks = c(1:10))+
                geom_hline(aes(yintercept=86.73))+
                geom_hline(aes(yintercept=84.36))+
                labs(title = "The Distribution of Scores in 10 Classes") + 
                theme(plot.title = element_text(hjust = 0.5,face="bold", size = 20)) +
                coord_flip() + 
                theme(axis.title.x = element_text(size = 12,face = "bold"), axis.title.y = element_text(size = 13,face = "bold")) + 
                layer(geom="point", stat = "identity", position="jitter") +
                scale_y_continuous(breaks = c(25,50,75,86.73,84.36))
         }

The_Distribution_of_Top50_in_10_Classes <- function(){
        ggplot(data = top50_count, mapping = aes(x = '', y = Count, fill = Class)) + 
                geom_bar(stat = 'identity', position = 'stack', width = 1) + 
                coord_polar(theta = 'y') + 
                labs(x = '', y = '', title = '') + 
                theme(axis.text = element_blank()) + 
                theme(axis.ticks = element_blank()) + 
                geom_text(aes(y = top50_count$Count/2 + c(0, cumsum(top50_count$Count)[-length(top50_count$Count)]), x = sum(top50_count$Count)/30, label = label)) + 
                labs(title = "The Distribution of Top50 in 10 Classes") +
                theme(plot.title = element_text(hjust = 0.5,face = "bold", size = 20)) +
                theme(panel.grid =element_blank()) 
}

The_Boxplot_of_the_Distribution_of_Scores_in_10_Classes <- function(){
        ggplot(data,aes(y = as.numeric(as.character(Scores)))) + 
                geom_boxplot() + 
                facet_grid(.~Class) + 
                labs(x = "", y = "Scores", title = "The Boxplot of the Distribution of Scores in 10 Classes") + 
                theme(plot.title = element_text(hjust = 0.5,face="bold",size = 20)) + 
                theme(axis.ticks.x = element_blank()) + 
                theme(axis.text.x = element_blank()) + 
                scale_y_continuous(breaks = c(25, 50, 75, 80,85,90)) +
                theme(axis.title.y = element_text(size=12, face="bold"))
}
