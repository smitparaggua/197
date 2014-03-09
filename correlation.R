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
data = toNumeric(data.raw)
data = as.data.frame(data)

data.pial = data[,grep("pial", names(data))]

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

# get names of the rows and columns, p-value, Cramer's V
names.col = colnames(data.pial.chi)
names.row = rownames(data.pial.chi)
related.rows.names = array(dim=length(related.rows))
related.cols.names = array(dim=length(related.cols))
related.cramers = array(dim=length(related.rows))
related.contingency = array(dim=length(related.rows))
related.phi = array(dim=length(related.rows))
for(i in 1:length(related.rows)) {
    related.cols.names[i] = names.col[related.cols[i]]
    related.rows.names[i] = names.row[related.rows[i]]
    a = data.pial[,related.rows[i]]
    b = data.pial[,related.cols[i]]
    c = assocstats(table(a,b,exclude=c(8,9)))
    related.phi[i] = c$phi;
    related.cramers[i] = c$cramer;
    related.contingency[i] = c$contingency;
}
relatedVars = rbind(related.cols.names,related.rows.names, related.phi, related.cramers, related.contingency);
rownames(relatedVars) = c("data1", "data2", "phi-coefficient", "cramer's V", "contingency coeff")
relatedVars = relatedVars[, order(relatedVars[4,], decreasing = T)]

# for verification
a = array(dim=length(related))
for(i in 1:length(related)) {
    a[i] = data.pial.chi[related.rows[i], related.cols[i]] < 0.05
}

# Cramer's V
a = data.pial[,related.rows[2]]
b = data.pial[,related.cols[2]]
c = assocstats(table(a,b,exclude=c(8,9)))

a = cor(data.pial, method = "kendall")


# compute for social networking users
snUser = 1:nrow(data);
snUser[which(data$act87 == 1 | data$act112 == 1)] = 1;
snUser[which(data$act87 != 1 & data$act112 != 1)] = 2;
# number of social networking users sharing their photos
length(which(snUser == 1 & data$pial1h == 1))
# association for 2 variables
# for individual
a = table(data$pial8i, data$pial11g)
assocstats(a)
apply(a, 2, prop.table)
# for group
a = apply(data[,grep("pial8[a-z]", names(data))], 2, table, snUser)
lapply(a,assocstats)
apply(a$pial7i, 2, prop.table)

# for checking for relations
a = "pial11"
relatedVars[,c(grep(a,relatedVars[1,]), grep(a, relatedVars[2, ]))]
