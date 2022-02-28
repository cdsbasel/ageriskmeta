general <- c(0.49, 0.41,	0.46,	0.18,	0.52,	0.53,	0.46)
financial <- c(0.59, 0.49, 0.46, 0.52, 0.35, 0.4,	0.4, 0.23, 0.45, 0.3,	0.28,	0.35,	0.26,	0.2, 0.42, 0.15, 0.25, 0.34, 0.31, 0.44)
driving <- c(0.48, 0.39, 0.53)
recreational <- c(0.47, 0.42,	0.52)
occupational <- c(0.44,	0.34,	0.45)
health <- c(0.43,	0.33,	0.41)
social <- c(0.41)

all <- c(general,financial,driving,recreational, occupational, health, social)
range(all)
#[1] 0.15 0.59
median(all)
#[1] 0.415
sd(all)
#[1] 0.1053334



