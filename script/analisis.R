# Package untuk visualisasi
library(ggplot2)

# Buat folder gambar kalau belum ada
if (!dir.exists("gambar")) {
  dir.create("gambar")
}

# Baca dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")

# Melihat ukuran dataset
cat("Ukuran dataset:\n")
print(dim(data))

# Melihat nama variabel
cat("\nNama variabel dalam dataset:\n")
print(names(data))

# Melihat struktur data
cat("\nStruktur dataset:\n")
str(data)

# Melihat beberapa data awal
cat("\nData awal:\n")
print(head(data))

# Cek missing value di semua kolom
missing_semua_variabel <- colSums(is.na(data))

cat("\nJumlah missing value tiap variabel:\n")
print(missing_semua_variabel)

# Cek missing value pada variabel utama
na_internet_awal <- sum(is.na(data$akses_internet))
na_ipm_awal <- sum(is.na(data$ipm))

cat("\nNA akses internet sebelum diolah:", na_internet_awal, "\n")
cat("NA IPM sebelum diolah:", na_ipm_awal, "\n")

# Mengisi missing value variabel utama dengan median
data$akses_internet[is.na(data$akses_internet)] <- median(data$akses_internet, na.rm = TRUE)
data$ipm[is.na(data$ipm)] <- median(data$ipm, na.rm = TRUE)

# Cek ulang setelah missing value diisi
cat("\nNA akses internet setelah diolah:", sum(is.na(data$akses_internet)), "\n")
cat("NA IPM setelah diolah:", sum(is.na(data$ipm)), "\n")

# Melihat jumlah data normal, missing value, dan outlier
cat("\nCatatan data:\n")
print(table(data$catatan_data))

# Statistik deskriptif akses internet
cat("\nRingkasan akses internet:\n")
print(summary(data$akses_internet))

cat("\nStatistik tambahan akses internet:\n")
cat("Mean:", mean(data$akses_internet, na.rm = TRUE), "\n")
cat("Median:", median(data$akses_internet, na.rm = TRUE), "\n")
cat("Standar deviasi:", sd(data$akses_internet, na.rm = TRUE), "\n")
cat("Varians:", var(data$akses_internet, na.rm = TRUE), "\n")
cat("Kuartil:\n")
print(quantile(data$akses_internet, na.rm = TRUE))

# Statistik deskriptif IPM
cat("\nRingkasan IPM:\n")
print(summary(data$ipm))

cat("\nStatistik tambahan IPM:\n")
cat("Mean:", mean(data$ipm, na.rm = TRUE), "\n")
cat("Median:", median(data$ipm, na.rm = TRUE), "\n")
cat("Standar deviasi:", sd(data$ipm, na.rm = TRUE), "\n")
cat("Varians:", var(data$ipm, na.rm = TRUE), "\n")
cat("Kuartil:\n")
print(quantile(data$ipm, na.rm = TRUE))

# Grafik 1: jumlah missing value tiap variabel
missing_df <- data.frame(
  variabel = names(missing_semua_variabel),
  jumlah = as.numeric(missing_semua_variabel)
)

plot_missing <- ggplot(missing_df, aes(x = reorder(variabel, jumlah), y = jumlah)) +
  geom_col(fill = "#3498db") +
  coord_flip() +
  labs(
    title = "Jumlah Missing Value pada Setiap Variabel",
    x = "Variabel",
    y = "Jumlah Missing Value"
  ) +
  theme_minimal()

print(plot_missing)

ggsave(
  filename = "gambar/gambar_1_missing_value_variabel.png",
  plot = plot_missing,
  width = 8,
  height = 4,
  dpi = 300
)

# Grafik 2: jumlah data berdasarkan catatan_data
catatan_df <- as.data.frame(table(data$catatan_data))
colnames(catatan_df) <- c("catatan_data", "jumlah")

plot_catatan <- ggplot(catatan_df, aes(x = catatan_data, y = jumlah)) +
  geom_col(fill = "#e67e22") +
  labs(
    title = "Jumlah Data Berdasarkan Catatan Data",
    x = "Catatan Data",
    y = "Jumlah Data"
  ) +
  theme_minimal()

print(plot_catatan)

ggsave(
  filename = "gambar/gambar_2_catatan_data.png",
  plot = plot_catatan,
  width = 8,
  height = 4,
  dpi = 300
)

# Grafik 3: scatter plot akses internet dan IPM
plot_scatter <- ggplot(data, aes(x = akses_internet, y = ipm)) +
  geom_point(color = "#2980b9", alpha = 0.5) +
  geom_smooth(method = "lm", color = "#e74c3c", se = TRUE) +
  labs(
    title = "Hubungan Akses Internet dan IPM",
    x = "Akses Internet (%)",
    y = "IPM"
  ) +
  theme_minimal()

print(plot_scatter)

ggsave(
  filename = "gambar/gambar_3_scatter_akses_internet_ipm.png",
  plot = plot_scatter,
  width = 8,
  height = 4,
  dpi = 300
)

# Grafik 4: histogram IPM
plot_histogram_ipm <- ggplot(data, aes(x = ipm)) +
  geom_histogram(bins = 30, fill = "#3498db", color = "white") +
  labs(
    title = "Distribusi Nilai IPM",
    x = "IPM",
    y = "Frekuensi"
  ) +
  theme_minimal()

print(plot_histogram_ipm)

ggsave(
  filename = "gambar/gambar_4_histogram_ipm.png",
  plot = plot_histogram_ipm,
  width = 8,
  height = 4,
  dpi = 300
)

# Grafik 5: boxplot akses internet dan IPM
data_boxplot <- data.frame(
  variabel = rep(c("Akses Internet", "IPM"), each = nrow(data)),
  nilai = c(data$akses_internet, data$ipm)
)

plot_boxplot <- ggplot(data_boxplot, aes(x = variabel, y = nilai, fill = variabel)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Boxplot Akses Internet dan IPM",
    x = "Variabel",
    y = "Nilai"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_boxplot)

ggsave(
  filename = "gambar/gambar_5_boxplot_akses_internet_ipm.png",
  plot = plot_boxplot,
  width = 8,
  height = 4,
  dpi = 300
)

# Uji normalitas IPM
shapiro_test <- shapiro.test(data$ipm)

cat("\nHasil uji normalitas Shapiro-Wilk:\n")
print(shapiro_test)

# Uji korelasi akses internet dan IPM
korelasi_test <- cor.test(
  data$akses_internet,
  data$ipm,
  method = "pearson"
)

cat("\nHasil uji korelasi Pearson:\n")
print(korelasi_test)

# Ringkasan hasil penting
cat("\nRingkasan hasil:\n")
cat("Koefisien korelasi:", korelasi_test$estimate, "\n")
cat("P-value korelasi:", korelasi_test$p.value, "\n")
cat("P-value normalitas:", shapiro_test$p.value, "\n")

