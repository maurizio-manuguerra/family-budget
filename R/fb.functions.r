test <- function(){
    #categories = c("Alcohol","Bills","CafeRestaurant","Car","Cash","ChildCare","Credit Card","Food","Health","Holidays","Lavori","Life","Mortgage","PayPal","Phone","Rent","Salary","School","Taxes","To refund","WOOLWORTHS")
    x <- categorise("AFG.csv")
    x <- categorise("Bankwest.csv", prev=x)
    str(x)
    #report(x)
}



show_category <- function(object, category){
    x <- object$expenses
	ii=which(x$kind==category)
	if (length(ii)>0) {
		print(x[ii,])
		cat("\n---------------------------------------------------------------------------\n\n")
		cat("Total:",sum(x[ii,]$amount))
	} else {
		print("No results!")
	}
	
}

new_category <- function(cat){
    output <- dir.create(cat)
    if (output) {
        cat("Category created. You can now add items running fix_unknowns(object)")
    } else {
        cat("Something wrong happened")
    }
}

del_category <- function(cat){
    output <- system(paste("rm -fr ", cat))
    if (output) {
        cat("Category deleted. Remember to re-run categorise(object)\n")
    } else {
        cat("Something wrong happened")
    }
}

rename_category <- function(cat_old, cat_new){
    output <- system(paste("mv", cat_old, cat_new, sep=" "))
    if (output) {
        cat("Category renamed. Remember to re-run categorise(object)\n")
    } else {
        cat("Something wrong happened")
    }
}    


categorise <- function(filein, prev = NULL){
    x <- read.csv(filein)
    names(x) <- c("date", "description", "amount")
    x$kind=rep('?',nrow(x))
    x$date <- as.Date(x$date,format="%d/%m/%y")
    x <- x[order(x$date),]
	#FIXME check existence of folder Categories and create if not there
	if ("./Categories" %in% list.dirs(recursive=F)){
	    #list csv files
	    files = list.files("./Categories", pattern="*.csv")
        if (length(files) > 0){
            categories <- as.character(sapply(files, function(x)substr(x,1,nchar(x)-4) ))
        } else {
            cat("The folder 'Categories' is empty. You should run new_category() to create the categories files and their content.\n")
            return()
            #advise to run new_category() to create list of categories.
        }
	} else {
        #create Categories folder and advise to run new_category() to create list of categories.
        dir.create("Categories")
        cat("The folder 'Categories' is not present and it has been created for you. You should now run new_category() to create the categories files and their content.")
        return()
	}
	for (cat_name in categories){
		cat_content = read.csv(paste("Categories/",cat_name,".csv",sep=''),header=F)[,1]
		for (icat in cat_content){
            ii=grep(icat,x$description,ignore.case=TRUE)
            x$kind[ii]=cat_name
        }
	}
	if (!is.null(prev)){
	    x <- rbind(prev$expenses, x)
	    x <- x[order(x$date),]
	    filein <- c(prev$filein, filein)
	}
	return(list(expenses=x, categories=categories, file=filein))
}

report <- function(object){
    x <- object$expenses
    categories <- object$categories
	total=0
	print(paste("Period ",min(x$date)," - ",max(x$date),sep=''))
	print("---------------------------------------------------")
	for (ilev in categories){
		ii=which(x$kind==ilev)
		print(paste(ilev,":",sum(x$amount[ii])))
		total=total+sum(x$amount[ii])
	}
	ii=which(x$kind=="?")
	print(paste("?:",sum(x$amount[ii])))
	total=total+sum(x$amount[ii])	
	print(paste("Total=",total))
}


show_unknowns <- function(object){
    x <- object$expenses
	ii=which(x$kind=="?")
	if (length(ii)>0) {
		print(x[ii,])
	} else {
		print("No unknowns!")
	}
	
}


fix_unknowns <- function(object){
    x <- object$expenses
    categories <- object$categories
    filein <- object$file
	unknowns <- which(x$kind == "?")
	if (length(unknowns) == 0) {
	    cat("No unknown records found.")
	    return(object)
	}
	len_c <- length(categories)
	for (i in unknowns){
    	cat("\n\n\n")
    	print(x[i,])
    	cat("\n", paste(1:len_c,categories,"\n"))
    	input <- readline(paste("Please choose category [1-",len_c,", s to skip, q to quit]: ",sep=''))
    	if (input == "q"){
    		cat("\nQuitting...")
    		return(object)
    	} else if (input == "s"){
    		cat("\nSkipping...")
    		break()
    	} else {
        	cat_name <- categories[as.numeric(input)]
    		write.table(x[i,2],file=paste('Categories/',cat_name,'.csv',sep=''),row.names=F,append=T,col.names=F)
    		x[i, "kind"] <- cat_name
    	}
    }
	return(list(expenses=x, categories=categories, file=filein))
}

