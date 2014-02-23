library(vcd);
ALPHA = 0.05

toNumeric <- function(vector) {
    vector.character = apply(X=vector, MARGIN=2, FUN=as.character, na.omit=T);
    vector.numeric = apply(X=vector.character, MARGIN=2, FUN=as.numeric, na.omit=T);
    return(vector.numeric);
}

chiSquared <- function(x, y, exclude=NA) {
    temp = table(x,y,exclude=exclude)
    return(chisq.test(temp))
}

data.raw = read.csv("anonymity_clean.csv", row.names = 1)

data.pial = data[,grep("pial", names(data))]
data.pial = toNumeric(data.pial)

# pial4 pial5
a = chiSquared(data.pial[,23], data.pial[,24], exclude=c(8,9))

# all chi-squared values
data.pial.chi = matrix(nrow = ncol(data.pial), ncol = ncol(data.pial))
colnames(data.pial.chi) = colnames(data.pial)
rownames(data.pial.chi) = colnames(data.pial)
for(i in 1:ncol(data.pial)) {
    for(j in 1:i) {
        a = chiSquared(data.pial[,i], data.pial[,j], exclude=c(8,9));
        data.pial.chi[i, j] = a$p.value;
    }
}

# get rows and columns that are related based on chi squared's p-value
related = which(data.pial.chi < ALPHA)
related.rows = related %% nrow(data.pial.chi);
related.cols = related %/% nrow(data.pial.chi) + 1;
related.cols[which(related.rows == 0)] = related.cols[which(related.rows == 0)] - 1;
related.rows[which(related.rows == 0)] = nrow(data.pial.chi);
a = which(related.cols == related.rows)
related.cols = related.cols[-a]
related.rows = related.rows[-a]

# get names of the rows and columns
a = colnames(data.pial.chi)
b = rownames(data.pial.chi)
related.rows.names = array(dim=length(related.rows))
related.cols.names = array(dim=length(related.cols))
for(i in 1:length(related.rows)) {
    related.cols.names[i] = a[related.cols[i]]
    related.rows.names[i] = b[related.rows[i]]
}
relatedVars = rbind(related.cols.names,related.rows.names, data.pial.chi[related]);

# for verification
a = array(dim=length(related))
for(i in 1:length(related)) {
    a[i] = data.pial.chi[related.rows[i], related.cols[i]] < 0.05
}

# Cramer's V
a = data.pial[,related.rows[2]]
b = data.pial[,related.cols[2]]
c = assocstats(table(a,b,exclude=c(8,9)))
relatedVars = relatedVars[]

a = cor(data.pial, method = "kendall")
