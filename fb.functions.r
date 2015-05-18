categories = c(
	"Alcohol",
	"Bills",
	"CafeRestaurant",
	"Car",
	"Cash",
	"ChildCare",
	"Credit Card",
	"Food",
	"Health",
	"Holidays",
	"Lavori",
	"Life",
	"Mortgage",
	"PayPal",
	"Phone",
	"Rent",
	"Salary",
	"School",
	"Taxes",
	"To refund",
	"WOOLWORTHS"
	)
	
#x <- categorise("Bankwest.csv", categories)
#report(x, categories)



show_category <- function(x, category){
	ii=which(x$kind==category)
	if (length(ii)>0) {
		print(x[ii,])
		cat("\n---------------------------------------------------------------------------\n\n")
		cat("Total:",sum(x[ii,]$amount))
	} else {
		print("No results!")
	}
	
}


categorise <- function(filein, categories){
    x <- read.csv(filein)
    names(x) <- c("date", "description", "amount")
    x$kind=rep('?',nrow(x))
    x$date <- as.Date(x$date,format="%d/%m/%y")
    x <- x[order(x$date),]
	#FIXME check existence of folder Categories and create if not there
	for (cat_name in categories){
		cat_content = read.csv(paste("Categories/",cat_name,".csv",sep=''),header=F)[,1]
#		sapply(cat_content, categ, kindstr=cat_name)
        ii <- grep(cat_content, x$description, ignore.case=TRUE)
        x$kind[ii]=cat_name
#		for (icat in cat_content){
#            ii=grep(icat,x$description,ignore.case=TRUE)
#            x$kind[ii]=cat_name
#        }
	}
	return(x)
}

report <- function(x,categories){
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

categ <- function(x, substr, kindstr){
	ii=grep(substr,x$description,ignore.case=TRUE)
	x$kind[ii]=kindstr
	x
}

show_unknowns <- function(x){
	ii=which(x$kind=="?")
	if (length(ii)>0) {
		print(x[ii,])
	} else {
		print("No unknowns!")
	}
	
}

fix_unknowns <- function(x, categories){
	next_index <- i <- which(x$kind == "?")[1]
	if (length(next_index) == 0) return(x)
	len_c <- length(categories)
	cat("\n\n\n")
	print(x[i,])
	cat("\n", paste(1:len_c,categories,"\n"))
	input <- readline(paste("Please choose category [1-",len_c,", s to skip, q to quit]: ",sep=''))
	if (input == "q"){
		cat("\nQuitting...")
		return(x)
	} else if (input == "s"){
		cat("\nSkipping...")
		x[i,]$kind <- '??'
		x = add_item_to_category(x, categories)
	} else {
		#x[i,]$kind <- categories[as.numeric(input)]
		write.table(x[i,2],file=paste('Categories/',categories[as.numeric(input)],'.csv',sep=''),row.names=F,append=T,col.names=F)
		x = categorise(x, categories)
		x = add_item_to_category(x, categories)
	}
	indices <- which(x$kind=="??")
	if (length(indices)>0) x[indices,]$kind = "?"
	return(x)
}

