install.packages("rmarkdown")
#matrix
library(rmarkdown)
1:20

bil <- 1:20
matriks.bil<-matrix(bil)
#berdasarkan kolom
matriks.bil<-matrix(bil,nrow= 4)
#berdasarkan baris
matriks.bil<-matrix(bil,byrow= T, nrow=4)

#saham
TLKM <- c(3000,3100,3050,3020,3200)
KLBF<-c(324,343,300,321,355)

saham<- c(TLKM,KLBF)
saham.matrix<-matrix(saham, byrow = T,nrow=2)

#mengubah nama di header kolom dan baris
hari <- c("senin","selasa","rabu","kamis","jumat")
nama.saham <- c("TLKM","KLBF")

#perintah kolom dan baris
colnames(saham.matrix)<-hari    #kolom
rownames(saham.matrix)<-nama.saham     #baris

#menambah baris dan kolom
BBCA<-c(1501,15010,1490,1520,1500)
#cbind(): menggabungkan objek R berdasarkan kolom
#rbind(): menggabungkan objek R berdasarkan baris
#rownames(): mengambil atau menetapkan nama-nama baris dari objek seperti-matriks
#colnames(): mengambil atau menetapkan nama-nama kolom dari objek seperti-matriks   `
#t(saham.baru) transpose
saham.baru<-rbind(saham.matrix,BBCA)

#menjumlahkan baris kolom
colSums(saham.baru)
rowSums(saham.baru)

#ratabaris kolom
rowMeans(saham.baru)
colMeans(saham.baru)

Rataan <-rowMeans(saham.baru)
saham.baru<-cbind(saham.baru,Rataan)
saham.baru


#operasi matrix/aritmatika
bil <-matrix(1:25,byrow=T, nrow=5)
bil+bil
bil*bil
bil%*%bil #(pekalian aljabar linier%6% )
1/bil
bil^3

#slicing indexing
#bil[baris,kolom]
bil[1,]
bil[4,5]
bil[4:5,]
bil[c(2,4),]
bil[c(1,3,4),]
bil[c(1,3,4),c(2,4,5)]
bil[1:2,1:3]


#atau secara langsung
data <- matrix(
  data = c(1,2,3, 11,12,13), 
  nrow = 2, byrow = TRUE,
  dimnames = list(c("row1", "row2"), 
                  c("C.1", "C.2", "C.3"))
)
data

#data: vektor data opsional
#nrow, ncol: jumlah baris dan kolom yang diinginkan, masing-masing.
#byrow: nilai logis. Jika FALSE (default) matriks diisi oleh kolom, jika tidak, matriks diisi oleh baris.
#dimnames: Daftar dua vektor yang memberikan nama baris dan kolom masing-masing.   `

# mengetahui jumlah kolom dan baris
ncol(saham.baru)
nrow(saham.baru)

#memilih subset
saham.baru[1,]
is.na(saham.baru)
