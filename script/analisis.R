# Package untuk visualisasi
library(ggplot2)

# Buat folder gambar kalau belum ada
if (!dir.exists("gambar")) {
  dir.create("gambar")
}

# Baca dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")

# Cek missing value di semua kolom
cat("Jumlah missing value tiap variabel:\n")
print(colSums(is.na(data)))

# Cek missing value pada variabel utama
na_internet_awal <- sum(is.na(data$akses_internet))
na_ipm_awal <- sum(is.na(data$ipm))

cat("\nNA akses internet sebelum diolah:", na_internet_awal, "\n")
cat("NA IPM sebelum diolah:", na_ipm_awal, "\n")

# Mengisi missing value dengan median
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

# Statistik deskriptif IPM
cat("\nRingkasan IPM:\n")
print(summary(data$ipm))

# Scatter plot akses internet dan IPM
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
  filename = "gambar/scatter_akses_internet_ipm.png",
  plot = plot_scatter,
  width = 8,
  height = 4,
  dpi = 300
)

# Histogram IPM
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
  filename = "gambar/histogram_ipm.png",
  plot = plot_histogram_ipm,
  width = 8,
  height = 4,
  dpi = 300
)

# Uji normalitas IPM
shapiro_test <- shapiro.test(data$ipm)

cat("\nHasil uji normalitas:\n")
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

