tab = read.csv("File_names_correspondence.csv", head=F, sep=";")
for (i in 1:dim(tab)[1])
	{
		file.rename(tab[i,2],tab[i,1])
	}
