data = read.csv("./Omnibus_July_2013_Anonymity_csv.csv");

getFrequencyPercentageOfSequence <- function(data, sequence) {
    result = NULL;
    for(i in sequence) {
        result = c(result, sum(i==data, na.rm=T));
    }
    temp = sum(result);
    result = result/temp;
    return(result);
}

saveBarplotWithLegend <- function(data, filename, main=NULL, names.arg=NULL, legendNames) {
    if(is.null(nrow(data))) {
        numberOfColors = length(data);
        legendX = length(data) + 1;
    } else {
        numberOfColors = nrow(data);
        legendX = length(data) + ncol(data);
    }
    colors=c("red", "blue", "green", "brown", "orange", "yellow", "violet");
    png(paste("plots/", filename,".png", sep=""), height=300,width=700);
    colors = colors[1:numberOfColors];
    par(xpd=T, mar=par()$mar+c(0,0,0,7))
    barplot(data, beside=T, main=main, col=colors, names.arg=names.arg);
    #legend(length(data) + ncol(data), .5, legendNames, fill=colors);
    legend(legendX, .7*max(data), legendNames, fill=colors);
    par(mar=c(5, 4, 4, 2) + 0.1)
    dev.off();
}

saveBarplot <- function(data, filename, main=NULL, names.arg=NULL, legendNames=NULL) {
    colors=c("red", "blue", "green", "brown", "orange", "yellow", "violet");
    if(is.null(ncol(data))) {
        png(paste("plots/", filename,".png", sep=""), height=300, width=400);
        barplot(data, beside=T, main=main, col=colors , names.arg=names.arg, cex.names=0.8);
        dev.off();
    } else {
        saveBarplotWithLegend(data=data, filename=filename, main=main, names.arg=names.arg, legendNames=legendNames);
    }
}

createPlotAndGetFrequency <- function(data, namePattern, choices, names=NULL, main=NULL, ...) {
    data = data[,grep(namePattern, names(data))];
    if(is.null(dim(data))) {
        data.frequency = getFrequencyPercentageOfSequence(data, choices);
    } else {
        data.frequency = apply(X=data, MARGIN=2, FUN=getFrequencyPercentageOfSequence, choices);
        #names = NULL;
    }
    saveBarplot(data.frequency, filename=namePattern, main=main, names.arg=names, legendNames=legendNames);
    return(data.frequency);
}

# delete ql1 column because pialql1 and ql1 are the same
data$ql1 = NULL;

# since sample=2 means the person have been contacted through phone.
# so we place a value of 1 (yes) in pialql1
data$pialql1[which(data$sample == 2)] = 1;

# the majority of the questions are only asked for samples with
# intuse = 1 or smart3 = 1. so we delete other entries based on these values
# because they have too many NAs
data = data[-which(!(data$intuse == 1 | data$smart3 == 1)),];
data = data[-which(data$smart3 %in% NA & data$intuse != 1),];
write.csv(data, "anonymity_clean.csv");

data = read.csv("./anonymity_clean.csv");

# create visualizations
columns = 7:79;
generalChoices = c(1,2,8,9);
generalLabel = c("yes", "no", "not sure", "refused");

data.frequency = NULL;
names = NULL;

namesToPlot = c("pial3[a-c]", "pial3[d-f]", "pial3[g-i]");
names$'pial3[a-c]' = c("websites\nbrowsed", "location while\nusing net", "content\ndownloaded");
names$'pial3[d-f]' = c("times of\nday online", "applications/programs\nyou use", "search\nhistory");
names$'pial3[g-i]' = c("content of\nemail", "people you exchange\nemail with", "content of\nchats/hangouts");
mainLabels = namesToPlot;
mainLabels$'pial3[a-c]' = c("How much do you care that only you and those you\nauthorize should have access to the following information?");
mainLabels$'pial3[d-f]' = mainLabels$'pial3[a-c]';
mainLabels$'pial3[g-f]' = mainLabels$'pial3[a-c]';
choices = c(1,2,3,4,5,8,9);
legendNames = c("very important", "somewhat important", "not too important", "not at all important", "doesn't apply", "not sure", "refused");
for(i in namesToPlot) {
    data.frequency[[i]] = createPlotAndGetFrequency(data, namePattern=i, choices=choices, legendNames=legendNames, names=names[[i]], main=mainLabels[[i]]);
}

namesToPlot = c("pial1[a-d]", "pial1[e-h]","pial1[i-k]", "pial6", "pial7[a-d]", "pial7[e-h]","pial7[i-k]", "pial8[a-d]", "pial8[e-h]","pial8[i-k]");
names$'pial1[a-d]' = c("email\naddress", "home\naddress", "home phone\nnumber", "cell phone\nnumber");
names$'pial1[e-h]' = c("employer or company\nyou work for", "political party\naffiliation", "written with your\nname on it", "photo\nof you");
names$'pial1[i-k]' = c("video\nof you", "organizations/groups\nyou belong to",  "date\nof birth")
names$pial6 = c("real name", "assoc name", "w/o revealing\nwho you are")
names$'pial7[a-d]' = c("temporary\nusername/email", "fake name", "misleading\ninformation", "off cookies")
names$'pial7[e-h]' = c("clear hist", "service for\nanonymity", "encrypted\ncommunication", "avoid sites ask\nfor real name");
names$'pial7[i-k]' = c("deleted/edited\npost", "ask someone delete\npost about you", "public computer\nbrowse anonymously")
names$'pial8[a-d]' = c("family\nmembers", "certain\nfriends", "employer/supervisor/\ncoworkers", "site admin");
names$'pial8[e-h]' = c("hackers/criminals", "law enforcers", "people criticize/\nharass/target you", "comp want pay\non your DL")
names$'pial8[i-k]' = c("people from\nyour past", "advertisers", "government")
legendNames = c("yes", "no", "doesn't apply", "not sure", "refused");
mainLabels = namesToPlot;
mainLabels$'pial1[a-d]' = "Which information about you is available on the\ninternet for others to see?"
mainLabels$'pial1[e-h]' = mainLabels$'pial1[a-d]';
mainLabels$'pial1[i-k]' = mainLabels$'pial1[a-d]';
mainLabels$'pial7[a-d]' = "While using the internet, have you ever done\nany of the following?";
mainLabels$'pial7[e-h]' = mainLabels$'pial7[a-d]';
mainLabels$'pial7[i-k]' = mainLabels$'pial7[a-d]';
mainLabels$'pial8[a-d]' = "Have you ever tried to use the internet in ways that keep the following\nfrom being able to see what you have read/watched/posted online?";
mainLabels$'pial8[e-h]' = mainLabels$'pial8[a-d]';
mainLabels$'pial8[i-k]' = mainLabels$'pial8[a-d]';
for(i in namesToPlot) {
    data.frequency[[i]] = createPlotAndGetFrequency(data, namePattern=i, choices=c(1,2,3,8,9), legendNames=legendNames, names=names[[i]], main=mainLabels[[i]]);
}

namesToPlot = c("pial5", "pial4", "pial2", "pial9[a-d]", "pial9[e-g]", "pial9[h-j]", "pial11[a-d]", "pial11[e-h]", "pial13");
names$pial5 = generalLabel;
names$pial4 = generalLabel;
names$pial2 = generalLabel;
names$pial13 = generalLabel;
names$'pial9[a-d]' = c("change privacy\nsettings", "hide some updates\nto certain people", "delete people\nfrom network", "remove name\ntagged photos");
names$'pial9[e-g]' = c("delete comment\nof others", "ask someone remove\ninfo about you", "diff. profile for\ndiff. people");
names$'pial9[h-j]' = c("ignore/refuse\nfriend request", "block/unfriend\nsomeone", "delete something\nyou posted (past)");
names$'pial11[a-d]' = c("important personal\ninfo stolen", "account compromised\nw/o permission", "victim of online\nscam; lost money", "stalked/harassed\n online");
names$'pial11[e-h]' = c("lost job/educ\nbec of your post", "rel problem bec\nof your post", "reputation damaged\n(from online activity)", "physical danger\nfrom online activity");
mainLabels = namesToPlot;
mainLabels$'pial9[a-d]' = "Do you ever do the following?"
mainLabels$'pial9[e-g]' = mainLabels$'pial9[a-d]';
mainLabels$'pial9[e-g]' = mainLabels$'pial9[a-d]';
mainLabels$'pial9[e-g]' = mainLabels$'pial9[a-d]';
mainLabels$'pial11[a-d]' = "Have you ever experience any of the following as a result of your online activities?"
mainLabels$'pial11[e-h]' = mainLabels$'pial11[a-d]'
legendNames = c("yes", "no", "not sure", "refused");
for(i in namesToPlot) {
    data.frequency[[i]] = createPlotAndGetFrequency(data, namePattern=i, choices=generalChoices, names=names[[i]], legendNames=legendNames, main=mainLabels[[i]]);
}

names$pial10 = c("very\neasy", "somewhat\neasy", "not\ntoo easy", "almost\nimpossible", "not\nsure", "refused");
data.frequency$pial10 = getFrequencyPercentageOfSequence(data=data$pial10, sequence=c(1,2,3,4,8,9));
saveBarplot(data=data.frequency$pial10, filename="pial10", names = names$pial10, main="How easy do you think would be for\ncompanies to find out who you are\neven if you didn't use your real name?");

names$pial12 = c("reasonable\nprotection", "not good\nenough", "not\nsure", "refused");
data.frequency$pial12 = getFrequencyPercentageOfSequence(data=data$pial12, sequence=c(1,2,8,9));
saveBarplot(data=data.frequency$pial12, filename="pial12", main="pial12", names = names$pial12);

#todo social networking (WEB1-A)
