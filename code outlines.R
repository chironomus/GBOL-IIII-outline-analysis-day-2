require(Momocs)
require(gridExtra)
#set path to files
lf1 <- list.files('C:/Users/Viktor Baranov/Documents/Baranov/LMU/r crash course/masks', full.names=TRUE)

#path to files
#coo <- import_jpg(lf)#import the jpgs
coo <- import_jpg(lf1)#import
Out(coo)#conversion of jpegs to outlines


dat=Out(coo)# outlines saved as cash RAM object



panel(dat)#plotting outlines
#stack(dat)#stacking the shapes together


coo_plot(b)
coo_plot(coo_slidedirection(b, "up"))
coo_plot(coo_slidedirection(b, "right"))
coo_plot(coo_slidedirection(b, "left"))
coo_plot(coo_slidedirection(b, "down"))
b <- dat %>% slice(1:69) # slice the outlines 
stack(b)        #create stack of the outlines 


dat %>% slice(1:69)%>%
  coo_center %>% coo_scale %>%
  coo_alignxax()%>%
  stack(b) #centering and un-directioning stack    - that makes all your outlines to be size-independent



dat.ld=dat      #here we are going to add labells to the every chaincode of the outline
#this is needed to communicate certain property of every specimen, here it is a type of the habitat
#Xylophagous, terrrestrial, aquatic or "unknown"

dat.ld$fake <- factor(c("Xylophagous",
                        "Xylophagous",
                        "Xylophagous",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "Xylophagous",
                        "aquatic",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "unknown",
                        "terrestrial",
                        "unknown",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "terrestrial",
                        "terrestrial",
                        "unknown",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "unknown",
                        "unknown",
                        "unknown",
                        "unknown",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "aquatic",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "unknown",
                        "terrestrial",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "aquatic",
                        "terrestrial",
                        "unknown",
                        "terrestrial",
                        "unknown",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial",
                        "terrestrial"))

#another label - specimen is either extant or fossil
dat.ld$fake2 <- factor(c(	"extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "burmese",
                          "extant",
                          "messel",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "burmese",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "baltic",
                          "burmese",
                          "burmese",
                          "burmese",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "slovenian shales",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          "extant",
                          " Limestone Montsech",
                          "extant",
                          "dominican",
                          "extant",
                          "extant",
                          "extant",
                          "extant"))
#id of the fossils       - add additional identifiers (which particular fossil deposit it coming from)
dat.ld$fake3 <- factor(c("extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "Liu et al. 2019 mt 1",
                         "extant",
                         "Messel specimen",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "NHMLA spec 2",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "ped01",
                         "ped0041",
                         "ped0362",
                         "ped0031",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "slovenian shales",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         "extant",
                         " Limestone Montsech",
                         "extant",
                         "dominican",
                         "extant",
                         "extant",
                         "extant",
                         "extant"))
#calculating harmonics
coo_oscillo(dat.ld[1], "efourier")     #thats where actuall Fourier transformation occurs
Ptolemy(dat.ld[1])  #plotting the number of harmonics


dat.f <- efourier(dat.ld)
dat.f <- efourier(dat.ld, nb.h=36) #thats number of harmonics I used -36
dat.p <- PCA(dat.f)   #plot a PCA plot


m <- manova(dat.p$x[, 1:69] ~  dat.ld$fake2)    #run a MANOVA on the label graded factor
summary.aov(m)
#better PCA plot
plot(dat.p)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggfortify)
df_out <- as.data.frame(dat.p$x)

#plot PCA with different factors
plot(dat.p, dat.ld$fake,chull=TRUE, ellipses=TRUE, conf_ellipses = 0.9)
plot(dat.p, dat.ld$fake2,chull=TRUE, ellipses=TRUE, conf_ellipses = 0.9)
plot(dat.p, dat.ld$fake3,chull=TRUE, ellipses=TRUE, conf_ellipses = 0.9)
#see contribution of the different principle components
hist(dat.f, drop=0)
boxplot(dat.f, drop=1)
dat.p <- PCA(dat.f)
class(dat.p)        # a PCA object, let's plot it
plot(dat.p)

#


#yet better PCA
plot(dat.p, dat$fake)
plot(dat.p, dat.ld$fake, 1, pos.shp="circle", stars=TRUE, chull.filled=TRUE, palette=col_spring)

#Graphical representation of the PC contributions
PCcontrib(dat.p)

# K-means (how much the clusters in PCA are different)
dat.l <- LDA(dat.p,dat$fake)
MANOVA_PW(dat.p, "fake")
KMEANS(dat.p, centers = 10)
dat.f %>% mshapes() %>% coo_plot()


dat %>% efourier(norm=TRUE) %>% PCA() %>% plot("fake")
