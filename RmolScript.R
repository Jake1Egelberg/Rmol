tryCatch(find.package(c("r3dmol","this.path","shiny","htmlwidgets")),
         error=function(e) install.packages(c("r3dmol","this.path","shiny","htmlwidgets"),repos="http://lib.stat.cmu.edu/R/CRAN/"))
library(r3dmol)
library(this.path)
library(shiny)
library(htmlwidgets)

file.dir<-this.dir()
setwd(file.dir)

parms<-read.delim(paste(file.dir,"/Parms.txt",sep=""),sep=":")

#Select PDB
pdb.value<-trimws(parms[which(parms$RMOL.PARMS=="pdb.value"),]$VALUE, which=c("both"))

#Set style (cartoon, sphere, surface, stick)
style.value<-trimws(parms[which(parms$RMOL.PARMS=="style.value"),]$VALUE, which=c("both"))

#Amino acids
aspglu<-c("ASP","GLU")
cysmet<-c("CYS","MET")
serthr<-c("SER","THR")
phetyr<-c("PHE","TYR")
asngln<-c("ASN","GLN")
gly<-c("GLY")
leuvalile<-c("LEU","VAL","ILE")
ala<-c("ALA")
trp<-c("TRP")
his<-c("HIS")
pro<-c("PRO")
lysarg<-c("LYS","ARG")
amino.acids<-c(aspglu,cysmet,serthr,phetyr,asngln,gly,leuvalile,ala,trp,his,pro,lysarg)

#Print color scheme
colors<-c("Bright Red","Bright Red",
          "Yellow","Yellow",
          "Orange","Orange",
          "Mid Blue","Mid Blue",
          "Cyan","Cyan",
          "Light Grey",
          "Green","Green","Green",
          "Dark Grey",
          "Purple",
          "Pale Blue",
          "Flesh",
          "Tan","Tan")

amino.scheme<-data.frame(Residue=amino.acids,
                         Color=colors)

#Uses amino color scheme
if(style.value=="cartoon"){
    m_add_model(id=r3dmol(backgroundColor="white", cartoonQuality=10), data = m_fetch_pdb(pdb.value, save.pdb = FALSE), format = "pdb") %>%
      m_set_style(sel=m_sel(resn=aspglu),style=m_style_cartoon(color="#E60A0A",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=cysmet),style=m_style_cartoon(color="#E6E600",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=lysarg),style=m_style_cartoon(color="#145AFF",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=serthr),style=m_style_cartoon(color="#FA9600",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=phetyr),style=m_style_cartoon(color="#3232AA",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=asngln),style=m_style_cartoon(color="#00DCDC",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=gly),style=m_style_cartoon(color="#EBEBEB",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=leuvalile),style=m_style_cartoon(color="#0F820F",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=ala),style=m_style_cartoon(color="#C8C8C8",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=trp),style=m_style_cartoon(color="#B45AB4",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=his),style=m_style_cartoon(color="#8282D2",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=pro),style=m_style_cartoon(color="#DC9682",arrows=FALSE)) %>%
      m_set_style(sel=m_sel(resn=lysarg),style=m_style_cartoon(color="#BEA06E,arrows=FALSE")) %>%
      m_set_style(sel=m_sel(resn=amino.acids,invert=TRUE),style=m_style_stick()) %>%
      m_zoom_to() %>%
    saveWidget(file="IMG.html")
} else if(style.value=="sphere"){
  m_add_model(id=r3dmol(backgroundColor="white"), data = m_fetch_pdb(pdb.value, save.pdb = FALSE), format = "pdb") %>%
    m_set_style(sel=m_sel(resn=amino.acids),style=m_style_sphere(colorScheme="amino")) %>%
    m_set_style(sel=m_sel(resn=amino.acids,invert=TRUE),style=m_style_stick()) %>%
    m_zoom_to() %>%
    saveWidget(file="IMG.html")
} else if(style.value=="surface"){
  m_add_model(id=r3dmol(backgroundColor="white"), data = m_fetch_pdb(pdb.value, save.pdb = FALSE), format = "pdb") %>%
    m_set_style(sel=m_sel(resn=amino.acids),style=m_style_sphere(colorScheme="amino")) %>%
    m_add_surface(atomsel=m_sel(resn=amino.acids),style=m_style_surface(opacity=0.9,colorScheme="amino")) %>%
    m_set_style(sel=m_sel(resn=amino.acids,invert=TRUE),style=m_style_stick()) %>%
    m_zoom_to() %>%
    saveWidget(file="IMG.html")
} else if(style.value=="stick"){
  m_add_model(id=r3dmol(backgroundColor="white"), data = m_fetch_pdb(pdb.value, save.pdb = FALSE), format = "pdb") %>%
    m_set_style(sel=m_sel(resn=amino.acids),style=m_style_stick(colorScheme="amino")) %>%
    m_zoom_to() %>%
    saveWidget(file="IMG.html")
}

