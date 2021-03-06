#mike vlah (michael.vlah@duke.edu)
#2021-07-05
#datasets and plots for Bernhardt, Savoy et al. metabolism synthesis paper

library(plyr)
library(tidyverse)
library(RColorBrewer)
library(plotrix)
library(nhdR)

setwd('C:/Users/Alice Carter/git/loticlentic_synthesis/')

# 0: setup ####

dir.create('figures',
           showWarnings = FALSE)
dir.create('export_datasets',
           showWarnings = FALSE)
dir.create('output_data/spatial',
           showWarnings = FALSE)

#choose colors for plot categories
fnetcolor = 'sienna4'
spcolor = 'cadetblue4'
gppcolor = 'forestgreen'
ercolor = 'sienna'

source('src/plot_helpers.R')

# 1: load site data that will be used in plots (as opposed to stats) ####

site_data_1 = readRDS('data/phil_powell_data/lotic_site_info_filtered.rds') %>%
    as_tibble() %>%
    select(sitecode = Site_ID,
           Stream_PAR_sum, Disch_ar1, Disch_skew, MOD_ann_NPP)

# 2: load and prepare fluxnet data ####

restricted_use_sites = c("RU-Sam", "RU-SkP", "RU-Tks", "RU-Vrk", "SE-St1", "ZA-Kru")

#summarize by site, starting with a dataset that's summarized by site-year
# fnet_ann = readRDS('output_data/fluxnet_filtered_metabolism.rds')
# fnet_names = names(fnet_ann)

# for(i in 1:length(fnet_ann)){
#     fnet_ann[[i]]$sitecode = fnet_names[i]
# }
# 
# fnet_ann = fnet_ann[! fnet_names %in% restricted_use_sites]
# 
# fnet_full = Reduce(bind_rows, fnet_ann) %>%
#     as_tibble()
# 
# fnet_site = readRDS('output_data/fluxnet_site_info_filtered.rds') %>%
#     as_tibble() %>%
#     select(sitecode = Site_ID,
#            GPP_site_mean = ann_GPP,
#            ER_site_mean = ann_ER) %>%
#     filter(! is.na(GPP_site_mean),
#            ! is.na(ER_site_mean)) %>%
#     mutate(NEP_site_mean = GPP_site_mean + ER_site_mean) %>%
#     arrange(sitecode)
# 
# #summarize by DOY for lips plots
# fnet_lips = fnet_full %>%
#     select(sitecode, GPP, ER, DOY, Year) %>%
#     group_by(sitecode, DOY) %>%
#     summarize(GPP = mean(GPP, na.rm=TRUE), #average metab by day across years
#               ER = mean(ER, na.rm=TRUE)) %>%
#     ungroup() %>%
#     arrange(sitecode, DOY) %>%
#     rename(GPP_C_filled = GPP, ER_C_filled = ER) %>%
#     mutate(NEP_C_filled = GPP_C_filled + ER_C_filled)


# 3. load and prepare streampulse data ####

sp_list = readRDS('data/phil_powell_data/lotic_gap_filled.rds')

#summarize by site
sp_names = names(sp_list)

for(i in 1:length(sp_list)){
    sp_list[[i]]$sitecode = sp_names[i]
}

sp_full = Reduce(bind_rows, sp_list) %>%
    as_tibble() %>%
    select(sitecode, Date, Year, DOY, GPP_C, ER_C, GPP_C_filled, ER_C_filled) %>%
    arrange(sitecode, Date)

sp_site = sp_full %>%
    group_by(sitecode, Year) %>%
    summarize(GPP_ann_sum = sum(GPP_C_filled, na.rm = TRUE),
              ER_ann_sum = sum(ER_C_filled, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(sitecode) %>%
    summarize(GPP_site_mean = mean(GPP_ann_sum, na.rm = TRUE),
              ER_site_mean = mean(ER_ann_sum, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(sitecode) %>%
    mutate(NEP_site_mean = GPP_site_mean + ER_site_mean) %>%
    left_join(site_data_1, by = 'sitecode')

#summarize by DOY for lips plots
sp_lips = sp_full %>%
    select(sitecode, Year, DOY, GPP_C_filled, ER_C_filled) %>%
    group_by(sitecode, DOY) %>%
    summarize(GPP_C_filled = mean(GPP_C_filled, na.rm=TRUE), #average metab by day across years
              ER_C_filled = mean(ER_C_filled, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(sitecode, DOY) %>%
    mutate(NEP_C_filled = GPP_C_filled + ER_C_filled) %>%
    left_join(site_data_1, by = 'sitecode') #include site data

# 4: split sites by coverage ####

coverage_sp = sp_full %>%
    group_by(sitecode) %>%
    summarize(ndays = sum(! is.na(GPP_C)),
              nyears = length(unique(Year)),
              coverage = round(ndays / (nyears * 365),
                               digits = 2),
              .groups = 'drop')

# coverage_fnet = fnet_full %>%
#     group_by(sitecode) %>%
#     summarize(ndays = sum(! is.na(GPP)),
#               nyears = length(unique(Year)),
#               coverage = round(ndays / (nyears * 365),
#                                digits = 2),
#               .groups = 'drop')

sp_high_cov_sites = coverage_sp %>%
    filter(coverage >= 0.8) %>%
    pull(sitecode)
sp_high_cov_bool = sp_site$sitecode %in% sp_high_cov_sites

# fnet_high_cov_sites = coverage_fnet %>%
#     filter(coverage >= 0.8) %>%
#     pull(sitecode)
# fnet_high_cov_bool = fnet_site$sitecode %in% fnet_high_cov_sites

# 5: (skip if only plotting) generate site data that will be used for stats ####

site_data_2 = readRDS('output_data/lotic_site_info_filtered.rds') %>%
    as_tibble() %>%
    select(sitecode = Site_ID,
           lat = Lat,
           lon = Lon,
           epsg_crs, COMID, VPU)

#get reach proportional distance for as many sites as possible
got_reach_proportions <- file.exists('output_data/spatial/reach_proportion_reference.rds')

if(got_reach_proportions){
    site_data_2$reach_proportion = readRDS('output_data/spatial/reach_proportion_reference.rds')
} else {
    site_data_2$reach_proportion = NA
    remake_reach_proportion_column = FALSE
    errflag = FALSE
    reach_proportions_needed = which(is.na(site_data_2$reach_proportion))
    for(i in reach_proportions_needed){

        print(paste(i, length(reach_proportions_needed), sep = '/'))

        if(is.na(site_data_2$reach_proportion[i]) || remake_reach_proportion_column){

            tryCatch({
                rp = calc_reach_prop(VPU = site_data_2$VPU[i],
                                     COMID = site_data_2$COMID[i],
                                     lat = site_data_2$lat[i],
                                     long = site_data_2$lon[i],
                                     CRS = site_data_2$epsg_crs[i],
                                     quiet = TRUE,
                                     force_redownload = FALSE)
            }, error = function(e) {print('error'); errflag <<- TRUE} )

            if(errflag){
                errflag = FALSE
                site_data_2$reach_proportion[i] = NA
                next
            }

        } else {
            message('already got this one. set remake_reach_proportion_column = TRUE to get it again.')
            next
        }

        site_data_2$reach_proportion[i] = rp
    }

    saveRDS(site_data_2$reach_proportion, 'output_data/spatial/reach_proportion_reference.rds')
}

#retrieve NHDPlusV2 data (will be skipped if output_data/spatial/nhdplusv2_data.rds exists)

if(! file.exists('output_data/spatial/nhdplusv2_data.rds')){

    #construct list of DSN=component pairs to acquire. see NHDPlus docs for more.
    setlist = list('NHDPlusAttributes'='PlusFlowlineVAA',
                   'NHDPlusAttributes'='ElevSlope')

    #retrieve NHDPlusV2 data
    nhdplusv2_data = nhdplusv2_bulk(site_data_2, setlist, quiet=TRUE)

    #nhd variable names do not have consistent naming conventions. sometimes they're
    #all caps; other times camel case. here's a crude way to deal with that.
    colnames(nhdplusv2_data) = toupper(colnames(nhdplusv2_data))
    nhdplusv2_data = nhdplusv2_data[, ! duplicated(colnames(nhdplusv2_data))]

    #choose variables to join; filter dupes
    nhdplusv2_data = nhdplusv2_data %>%
        as_tibble() %>%
        select(COMID, STREAMORDE, FROMMEAS, TOMEAS,
               SLOPE, REACHCODE, AREASQKM, TOTDASQKM, MAXELEVSMO,
               MINELEVSMO) %>%
        group_by(COMID) %>%
        summarize_all(first) %>%
        ungroup()

    site_data_2 = left_join(site_data_2, nhdplusv2_data, by='COMID')

    #correct catchment area (AREASQKM) based on where each site falls within its reach.
    #use this to correct watershed area (TOTDASQKM) and to determine an areal
    #correction factor that can be multiplied with any areal summary data.
    site_data_2$AREASQKM_corr = round(site_data_2$AREASQKM * site_data_2$reach_proportion, 5)
    site_data_2$TOTDASQKM_corr = site_data_2$TOTDASQKM - (site_data_2$AREASQKM - site_data_2$AREASQKM_corr)
    site_data_2$areal_corr_factor = site_data_2$TOTDASQKM_corr / site_data_2$TOTDASQKM

    #save progress
    saveRDS(site_data_2, 'output_data/spatial/nhdplusv2_data.rds')
}

# 6: (Figure 1) GPP-ER biplot and dist plots ####

axis_cex = 2.4 #applies to labels and tick values

jpeg(width=10, height=10, units='in', res=300, quality=100, type='cairo',
     filename='figures/gpp_er_biplot_cumulAnnual.jpeg')
# par(mar=c(4.5, 4.5, 2, 2))
par(mar=c(6.5, 6.5, 2, 2))

log_gpp_fnet = log(fnet_site$GPP_site_mean)
log_er_fnet = log(fnet_site$ER_site_mean * -1) * -1
log_gpp_sp = log(sp_site$GPP_site_mean)
log_er_sp = log(sp_site$ER_site_mean * -1) * -1

gpptck = c(1, 10, 100, 1000, 10000)
# gpptck = c(0.1, 1, 10, 100, 1000, 10000)
ertck = rev(c(10000, 1000, 100, 10, 1))

# plot(log_gpp_fnet,
#      log_er_fnet, col=alpha(fnetcolor, alpha=0.5),
plot(log_gpp_sp[sp_high_cov_bool],
     log_er_sp[sp_high_cov_bool], col=alpha(fnetcolor, alpha=0.5),
     xlab='', ylab='', bg=alpha(fnetcolor, alpha=0.5),
     cex=1.5, cex.lab=axis_cex, cex.axis=axis_cex, ylim=-log(rev(range(ertck))),
     pch=21, yaxt='n', xaxt='n', xlim=log(range(gpptck)), lwd=2)
mtext(expression(paste("Cumulative GPP (gC"~"m"^"-2"*" y"^"-1"*')')),
      1, line=5, cex=axis_cex)
mtext(expression(paste("Cumulative ER (gC"~"m"^"-2"*" y"^"-1"*')')),
      2, line=3.5, cex=axis_cex)
# points(log_gpp_sp, log_er_sp, lwd=2,
#        col=alpha(spcolor, alpha=0.5), cex=1.5, pch=21, bg=alpha(spcolor, alpha=0.5))
points(log_gpp_sp[! sp_high_cov_bool], log_er_sp[! sp_high_cov_bool], lwd=2,
       col=alpha(spcolor, alpha=0.5), cex=1.5, pch=21, bg='transparent')
# points(log_gpp_fnet[! fnet_high_cov_bool], log_er_fnet[! fnet_high_cov_bool],
#        col=alpha(fnetcolor, alpha=0.5), cex=1.5, pch=21, bg='transparent', lwd=2)
points(log_gpp_sp[sp_high_cov_bool], log_er_sp[sp_high_cov_bool], lwd=2,
       col=alpha(spcolor, alpha=0.5), cex=1.5, pch=21, bg=alpha(spcolor, alpha=0.5))
legend('topright', legend=c('FLUXNET', 'StreamPULSE'), pch=21, bty='n', pt.cex=1.5,
       col=c(alpha(fnetcolor, alpha=0.5), alpha(spcolor, alpha=0.5)), pt.lwd=2,
       pt.bg=c(alpha(fnetcolor, alpha=0.5), alpha(spcolor, alpha=0.5)), x.intersp=2)
legend('topright', legend=c('FLUXNET', 'StreamPULSE'), pch=21, bty='n', pt.cex=1.5,
       col=c(alpha(fnetcolor, alpha=0.5), alpha(spcolor, alpha=0.5)),
       pt.bg='transparent', pt.lwd=2)
all_gpp = c(sp_site$GPP_site_mean)
all_gpp[all_gpp <= 0] = NA
gpprng = range(all_gpp, na.rm=TRUE)
all_er = c( sp_site$ER_site_mean)
all_er[all_er >= 0] = NA
errng = range(all_er, na.rm=TRUE)

gpptck_log = log(gpptck)
axis(1, at=gpptck_log, labels=gpptck, cex.axis=axis_cex, padj=0.4)
ertck_log = log(ertck) * -1
axis(2, at=ertck_log, labels=ertck * -1, cex.axis=axis_cex, padj=0.2)

abline(a=0, b=-1, lty=2)

dev.off()

#--- distplots

jpeg(width=8, height=8, units='in', res=300, quality=100, type='cairo',
     filename='figures/gpp_er_distplots.jpeg')

par(mfrow=c(2, 1), mar=c(2,1,1,1), oma=c(0, 0, 0, 0))

#GPP
# dens = density(na.omit(log_gpp_fnet))
# # dens = density(na.omit(log_gpp_fnet[fnet_high_cov_bool]))
# gpp_dens_fnet = tibble(x=dens$x, y=dens$y)
dens = density(na.omit(log_gpp_sp))
# dens = density(na.omit(log_gpp_sp[sp_high_cov_bool]))
gpp_dens_sp = tibble(x=dens$x, y=dens$y)

plot(gpp_dens_sp$x, gpp_dens_sp$y, type='n', ann=FALSE, xaxt='n', yaxt='n',
     bty='n', xlim=c(-2,9.5))
axis(1, at=gpptck_log, labels=gpptck, padj=-1.3, tick = TRUE, line=-0.2, tcl=-0.2)
mtext('GPP', 1, line=1)
# polygon(x=c(gpp_dens_fnet$x, rev(gpp_dens_fnet$x)),
#         y=c(gpp_dens_fnet$y, rep(0, nrow(gpp_dens_fnet))),
#         col=alpha(fnetcolor, alpha=0.7),
#         border=alpha(fnetcolor, alpha=0.7))
polygon(x=c(gpp_dens_sp$x, rev(gpp_dens_sp$x)),
        y=c(gpp_dens_sp$y, rep(0, nrow(gpp_dens_sp))),
        col=alpha(spcolor, alpha=0.7),
        border=alpha(spcolor, alpha=0.7))

#ER
# dens = density(na.omit(log_er_fnet))
# # dens = density(na.omit(log_er_fnet[fnet_high_cov_bool]))
# er_dens_fnet = tibble(x=dens$x, y=dens$y)
dens = density(na.omit(log_er_sp))
# dens = density(na.omit(log_er_sp[sp_high_cov_bool]))
er_dens_sp = tibble(x=dens$x, y=dens$y)

plot(er_dens_sp$x, er_dens_sp$y, type='n', ann=FALSE, xaxt='n', yaxt='n',
     bty='n', xlim=c(2, -9.5))
axis(1, at=ertck_log, labels=ertck, padj=-1.3, tick = TRUE, line=-0.2, tcl=-0.2)
mtext('ER', 1, line=1)
# polygon(x=c(rev(er_dens_fnet$x), er_dens_fnet$x),
#         y=c(rev(er_dens_fnet$y), rep(0, nrow(er_dens_fnet))),
#         col=alpha(fnetcolor, alpha=0.7),
#         border=alpha(fnetcolor, alpha=0.7))
polygon(x=c(er_dens_sp$x, rev(er_dens_sp$x)),
        y=c(rev(er_dens_sp$y), rep(0, nrow(er_dens_sp))),
        col=alpha(spcolor, alpha=0.7),
        border=alpha(spcolor, alpha=0.7))

dev.off()

#NEP
# nep_fnet = fnet_site$NEP_site_mean
nep_sp = sp_site$NEP_site_mean

jpeg(width=8, height=8, units='in', res=300, quality=100, type='cairo',
     filename='figures/nep_distplot.jpeg')

par(mfrow=c(2, 1), mar=c(2,1,1,1), oma=c(0, 0, 0, 0))

# dens = density(na.omit(nep_fnet))
# dens = density(na.omit(nep_fnet[fnet_high_cov_bool]))
# nep_dens_fnet = tibble(x=dens$x, y=dens$y)
dens = density(na.omit(nep_sp))
# dens = density(na.omit(nep_sp[sp_high_cov_bool]))
nep_dens_sp = tibble(x=dens$x, y=dens$y)

plot(nep_dens_sp$x, nep_dens_sp$y, type='n', ann=FALSE, xaxt='n', yaxt='n',
     bty='n', xlim=c(-1723, 2116))
axis(1, padj=-1.3, tick = TRUE, line=-0.2, tcl=-0.2)
mtext('NEP', 1, line=1)
# polygon(x=c(rev(nep_dens_fnet$x), nep_dens_fnet$x),
#         y=c(rev(nep_dens_fnet$y), rep(0, nrow(nep_dens_fnet))),
#         col=alpha(fnetcolor, alpha=0.7),
#         border=alpha(fnetcolor, alpha=0.7))
polygon(x=c(nep_dens_sp$x, rev(nep_dens_sp$x)),
        y=c(rev(nep_dens_sp$y), rep(0, nrow(nep_dens_sp))),
        col=alpha(spcolor, alpha=0.7),
        border=alpha(spcolor, alpha=0.7))

dev.off()

# 7: (Figures 2 and 3) lips plots and probability densities ####

dir.create('figures/lips', showWarnings = FALSE)
dir.create('figures/probdens', showWarnings = FALSE)

axis_cex = 2.4 #applies to labels and tick values

lips_plot = function(quant_filt=NULL, outfile, use_fnet=FALSE, ylims=NULL,
                     wee=FALSE){

    dset = if(use_fnet) fnet_lips else sp_lips
    dset_site = if(use_fnet) fnet_site else sp_site

    var_quant_filt = NULL
    if(! is.null(quant_filt)){
        quant_comp = strsplit(quant_filt, ' ')[[1]]
        qf = quantile(dset_site[, quant_comp[1], drop=TRUE], na.rm=TRUE,
        probs=as.numeric(quant_comp[3]))
        filt_sites = dset_site %>%
            filter_(paste(quant_comp[1], quant_comp[2], qf)) %>%
            pull(sitecode)
        dset = filter(dset, sitecode %in% filt_sites)

        var_quant_filt = paste0(quant_comp[1], ' ', quant_comp[2], ' ',
                                as.numeric(quant_comp[3]) * 100, '%')
    }

    nsites_included = length(unique(dset$sitecode))

    smry = dset %>%
        select(DOY, GPP_C_filled, ER_C_filled, NEP_C_filled) %>%
        group_by(DOY) %>%
        summarize_all(list(median=~median(., na.rm=TRUE),
                           quant25=~quantile(., na.rm=TRUE)[2],
                           quant75=~quantile(., na.rm=TRUE)[4])) %>%
        ungroup()

    jpeg(width=11, height=11, units='in', filename=outfile, type='cairo',
         res=300, quality=100)
    # pdf(file=outfile, width=10, height=10)

    if(! is.null(ylims)){

        # plot_dims = top_dims = dev.size('in')
        # top_dims[2] = plot_dims[2] * abs(ylims[2]) / sum(abs(ylims))

        gpplim = c(0, ylims[2])
        erlim = c(ylims[1], 0)

    } else {

        # top_dims = dev.size('in')

        gpplim=c(0, 5)
        erlim=c(-5, 0)

        if(use_fnet){
            gpplim[2] = 11
            erlim[1] = -7
        }
    }

    if(wee){
        axis_cex_ = axis_cex * 2
        line_ = -1.5
        par(mfrow=c(2, 1), oma=c(1, 1, 0, 0), mar=c(0, 5, 3, 1), lend=2)
    } else {
        axis_cex_ = axis_cex
        line_ = -2
        par(mfrow=c(2, 1), oma=c(1, 1, 0, 0), mar=c(0, 5, 3, 1), lend=2)
    }
    # fin=top_dims, new=TRUE)

    plot(smry$DOY, smry$GPP_C_filled_median, ylab='', yaxs='i', type='l',
         bty='n', lwd=4, xlab='', ylim=gpplim, xaxs='i', xaxt='n', yaxt='n',
         col='gray30')
    polygon(x=c(smry$DOY, rev(smry$DOY)),
            y=c(smry$GPP_C_filled_quant25, rev(smry$GPP_C_filled_quant75)),
            border=NA, col=alpha('forestgreen', alpha=0.6))
    axislocs = if(max(ylims) >= 10) seq(0, 10, 2) else c(0, 2.5, 5)
    axis(2, las=0, line=0, xpd=NA, tck=-.02, labels=FALSE,
         at=axislocs, cex.axis=axis_cex_, tcl=-0.3)
    # at=round(seq(0, gpplim[2], length.out=5), 1))
    axis(2, las=0, line=-0.5, xpd=NA, tcl=0, col='transparent',
         at=axislocs, cex.axis=axis_cex_)
    # at=round(seq(0, gpplim[2], length.out=5), 1))
    abline(h=0, lty=2, lwd=2, col='gray60')
    medsums = round(colSums(select(smry, contains('median'))), 1)

    if(! wee){
        mtext(expression(paste(bold("gC") ~ bold("m") ^ bold("-2") ~
                                   bold(" d") ^ bold("-1"))), side=2,
              line=line_, outer=TRUE, cex=axis_cex_)
    }

    # if(filter_label){
    #     legend('topright', title='Filters', bty='n', title.col='gray30',
    #            lty=1, seg.len=0.2, lwd=2, legend=c(..., var_quant_filt))
    # }

    legend('right', title='Cumulative\nMedian Sums', bty='n',
           legend=c(paste('GPP:', medsums[1]), paste('ER:', medsums[2]),
                    paste('NEP:', medsums[3])), title.col='gray30')
    legend('left', paste('Sites included:', nsites_included), bty='n')

    # if(! is.null(ylims)){
    #     plot_dims = bottom_dims = dev.size('in')
    #     bottom_dims[2] = plot_dims[2] * abs(ylims[1]) / sum(abs(ylims))
    # } else {
    #     bottom_dims = dev.size('in')
    # }

    par(mar=c(4, 5, 0, 1))#, fin=bottom_dims)
    if(wee){
        padj_ = 0.8
        DOY1 = ''
    } else {
        padj_ = 0.5
        DOY1 = 1
    }

    plot(smry$DOY, smry$ER_C_filled_median, ylab='', yaxs='i', type='l',
         bty='n', lwd=4, xlab='', ylim=erlim, xaxs='i', xaxt='n', yaxt='n')
    polygon(x=c(smry$DOY, rev(smry$DOY)),
            y=c(smry$ER_C_filled_quant25, rev(smry$ER_C_filled_quant75)),
            border=NA, col=alpha('sienna', alpha=0.6))
    if(max(ylims) >= 10){
        axislocs = seq(0, -10, -2)
        axisextra = -11
    } else {
        axislocs = c(0, -2.5, -5)
        axisextra = NULL
    }
    axis(2, las=0, line=0, xpd=NA, tck=-.02, labels=FALSE,
         # at=round(seq(0, erlim[1], length.out=5), 1))
         at=c(axislocs, axisextra), cex.axis=axis_cex_, tcl=-0.3)
    axis(2, las=0, line=-0.5, xpd=NA, tcl=0, col='transparent',
         # at=round(seq(0, erlim[1], length.out=5), 1))
         at=axislocs, cex.axis=axis_cex_)
    axis(1, line=0, tck=-.02, labels=FALSE, at=c(1, seq(60, max(smry$DOY), 60)),
         cex.axis=axis_cex_, tcl=-0.3)
    axis(1, line=-0.5, tcl=0, col='transparent', at=c(1, seq(60, max(smry$DOY), 60)),
         cex.axis=axis_cex_, padj = padj_, tcl=-0.3,
         labels = c(DOY1, seq(60, max(smry$DOY), 60)))
    lines(smry$DOY, smry$NEP_C_filled_median, col='black', lwd=4, xpd=NA, lend=1)

    if(! wee){
        mtext('DOY', side=1, line=3.5, font=2, cex=axis_cex_)
    }
    dev.off()
}

pdf_plot = function(var, outfile){

    jpeg(width=7, height=4, units='in', filename=outfile, type='cairo',
         res=300, quality=100)
    # pdf(outfile, height=4, width=7)

    vv = na.omit(sort(sp_site[[var]]))
    dens = density(vv)
    vq = quantile(vv, probs=c(0.25, 0.75))
    densdf = tibble(x=dens$x, y=dens$y)
    dens25 = dens75 = densdf
    dens25 = dens25[densdf$x <= vq[1], ]
    dens75 = dens75[densdf$x >= vq[2], ]
    plot(densdf$x, densdf$y, type='l', xlab='width', ylab='density', bty='l',
         col='gray50', lwd=2)
    polygon(x=c(dens25$x, rev(dens25$x)),
            y=c(dens25$y, rep(0, nrow(dens25))), col='gray50', border='gray50')
    polygon(x=c(dens75$x, rev(dens75$x)),
            y=c(dens75$y, rep(0, nrow(dens75))), col='gray50', border='gray50')

    dev.off()
}

#overall
lips_ylim = c(-11, 11)
lips_plot(quant_filt=NULL, outfile='figures/lips_overall_sp.jpeg',
          ylims=lips_ylim)
# lips_plot(quant_filt=NULL, outfile='figures/lips_overall_fnet.jpeg',
#           use_fnet=TRUE, ylims=lips_ylim)

# pdf_plot('Disch_ar1', 'figures/probdens/probdens_Qar1.jpeg')
pdf_plot('Disch_skew', 'figures/probdens/probdens_Qskew.jpeg')
pdf_plot('MOD_ann_NPP', 'figures/probdens/probdens_MODIS.jpeg')
pdf_plot('Stream_PAR_sum', 'figures/probdens/probdens_PAR.jpeg')

#subsets
lips_ylim = c(-5, 5)
# lips_plot(quant_filt='Disch_ar1 > 0.75', outfile='figures/lips/lips_Qar1_75.jpeg',
#           ylims=lips_ylim, wee=TRUE)
# lips_plot(quant_filt='Disch_ar1 < 0.25', outfile='figures/lips/lips_Qar1_25.jpeg',
#           ylims=lips_ylim, wee=TRUE)
lips_plot(quant_filt='Disch_skew > 0.75', outfile='figures/lips_Qskew_75.jpeg',
          ylims=lips_ylim, wee=TRUE)
lips_plot(quant_filt='Disch_skew < 0.25', outfile='figures/lips/lips_Qskew_25.jpeg',
          ylims=lips_ylim, wee=TRUE)
lips_plot(quant_filt='MOD_ann_NPP > 0.75', outfile='figures/lips/lips_MODIS_75.jpeg',
          ylims=lips_ylim, wee=TRUE)
lips_plot(quant_filt='MOD_ann_NPP < 0.25', outfile='figures/lips/lips_MODIS_25.jpeg',
          ylims=lips_ylim, wee=TRUE)
lips_plot(quant_filt='Stream_PAR_sum > 0.75', outfile='figures/lips/lips_PAR_75.jpeg',
          ylims=lips_ylim, wee=TRUE)
lips_plot(quant_filt='Stream_PAR_sum < 0.25', outfile='figures/lips/lips_PAR_25.jpeg',
          ylims=lips_ylim, wee=TRUE)

# 8: (Figure 4) bubble plots ####

dir.create('figures/bubble_plots', showWarnings = FALSE)

bubble_plot = function(xvar, comp, logx=FALSE, outfile){

    jpeg(width=8, height=8, units='in', filename=outfile, type='cairo',
         res=300, quality=100)
    # pdf(outfile, height=8, width=8)
    par(mar=c(5, 5, 4, 6))

    plotcol = case_when(comp == 'GPP_site_mean' ~ gppcolor,
                        comp == 'ER_site_mean' ~ ercolor,
                        comp == 'NEP_site_mean' ~ 'black')

    if(xvar == 'Stream_PAR_sum'){
        xxlab = 'Light Availability (Mean Annual Surface PAR)'
    } else {
        xxlab = 'MODIS NPP'
    }

    gpprng = range(sp_site$GPP_site_mean, na.rm=TRUE)
    if(comp == 'ER_site_mean'){
        xx = abs(sp_site[[comp]])
    } else {
        xx = sp_site[[comp]]
    }
    if(comp != 'NEP_site_mean'){
        rescaled = ((xx - gpprng[1]) /
                        (gpprng[2] - gpprng[1])) * (5 - 1) + 1
    } else {
        xxrng = range(xx, na.rm=TRUE)
        # rescaled = ((xx - xxrng[1]) /
        #                 (xxrng[2] - xxrng[1])) * (3) + 0.2
        rescaled = ((xx - -2000) /
                        (1000 - -2000)) * (4) + 0.5
    }

    xxvar = sp_site[[xvar]]
    if(logx){
        xxvar = log(xxvar)
        if(xvar == 'Stream_PAR_sum'){
            xlm = c(.5, 2.8)
        } else {
            xlm = c(3.95, 7.2)
        }
    } else {
        if(xvar == 'Stream_PAR_sum'){
            xlm = c(2, 15.5)
        } else {
            xlm = range(xxvar, na.rm=TRUE)
        }
    }

    plot(xxvar,
         sp_site$Disch_ar1, pch=21,
    # plot(xxvar[sp_high_cov_bool],
    #      sp_site$Disch_ar1[sp_high_cov_bool], pch=21,
         xlab=xxlab, xaxt='n',
         ylab='Predictability of Flow (Discharge AR-1 Coeff.)',
         col=alpha(plotcol, alpha=0.5), bty='o',
         ylim=c(0.1, 1), xlim=xlm,
         xpd=NA, main='', cex=rescaled, font.lab=2,
         # xpd=NA, main='', cex=rescaled[sp_high_cov_bool], font.lab=2,
         bg=alpha(plotcol, alpha=0.5))
    # points(xxvar[! sp_high_cov_bool],
    #        sp_site$Disch_ar1[! sp_high_cov_bool], pch=21,
    #        col=alpha(plotcol, alpha=0.5), bg='transparent',
    #        cex=rescaled[! sp_high_cov_bool])

    if(comp != 'NEP_site_mean'){
        legend('right', legend=c(expression(paste(0.01)), '', '',
                                 expression(paste(4000))),
               bty='n', pch=21, pt.bg='transparent', x.intersp=1.7,
               pt.cex=c(1, 2, 3, 5), col='gray30', xpd=NA,
               y.intersp=c(1, 2, 1.2, 1.6),
               inset=c(-0.16, 0), title='')
    } else {
        legend('right', legend=c(expression(paste(-2000)), '', '',
                                 expression(paste(1000))),
               bty='n', pch=21, pt.bg='transparent', x.intersp=1.7,
               pt.cex=seq(0.5, 4, length.out=4), col='gray30', xpd=NA,
               y.intersp=c(1, 2, 1.2, 1.6),
               inset=c(-0.18, 0), title='')
    }
    if(comp == 'GPP_site_mean'){
        legend('right', legend=c('','','',''),
               bty='n', pch=21, pt.bg='transparent', x.intersp=1.5,
               pt.cex=c(1, 2, 3, 5), col='transparent', xpd=NA,
               inset=c(-0.15, 0),
               title=expression(paste(bold('Cumul.\nAnnual\nGPP'))))
    } else if(comp == 'ER_site_mean'){
        legend('right', legend=c('','','',''),
               bty='n', pch=21, pt.bg='transparent', x.intersp=1.5,
               pt.cex=c(1, 2, 3, 5), col='transparent', xpd=NA,
               inset=c(-0.15, 0),
               title=expression(paste(bold('Cumul.\nAnnual\nER'))))
    } else {
        legend('right', legend=c('','','',''),
               bty='n', pch=21, pt.bg='transparent', x.intersp=1.5,
               pt.cex=c(1, 2, 3, 5), col='transparent', xpd=NA,
               inset=c(-0.15, 0),
               title=expression(paste(bold('Cumul.\nAnnual\nNEP'))))
    }

    if(logx){
        if(xvar == 'Stream_PAR_sum'){
            tcks = c(1, 2, 4, 8, 16)
        } else {
            tcks = c(200, 400, 800, 1600, 3200)
        }
        tcks_log = log(tcks)
        axis(1, at=tcks_log, labels=tcks)
    } else {
        axis(1)
    }

    dev.off()
}

bubble_plot(xvar='Stream_PAR_sum', comp='GPP_site_mean', logx=FALSE, outfile='figures/bubble_plots/PAR_GPP_linear.jpeg')
bubble_plot(xvar='Stream_PAR_sum', comp='GPP_site_mean', logx=TRUE, outfile='figures/bubble_plots/PAR_GPP_log.jpeg')
bubble_plot(xvar='Stream_PAR_sum', comp='ER_site_mean', logx=FALSE, outfile='figures/bubble_plots/PAR_ER_linear.jpeg')
bubble_plot(xvar='Stream_PAR_sum', comp='ER_site_mean', logx=TRUE, outfile='figures/bubble_plots/PAR_ER_log.jpeg')
bubble_plot(xvar='Stream_PAR_sum', comp='NEP_site_mean', logx=FALSE, outfile='figures/bubble_plots/PAR_NEP_linear.jpeg')
bubble_plot(xvar='Stream_PAR_sum', comp='NEP_site_mean', logx=TRUE, outfile='figures/bubble_plots/PAR_NEP_log.jpeg')
bubble_plot(xvar='MOD_ann_NPP', comp='GPP_site_mean', logx=FALSE, outfile='figures/bubble_plots/MODNPP_GPP_linear.jpeg')
bubble_plot(xvar='MOD_ann_NPP', comp='GPP_site_mean', logx=TRUE, outfile='figures/bubble_plots/MODNPP_GPP_log.jpeg')
bubble_plot(xvar='MOD_ann_NPP', comp='ER_site_mean', logx=FALSE, outfile='figures/bubble_plots/MODNPP_ER_linear.jpeg')
bubble_plot(xvar='MOD_ann_NPP', comp='ER_site_mean', logx=TRUE, outfile='figures/bubble_plots/MODNPP_ER_log.jpeg')
bubble_plot(xvar='MOD_ann_NPP', comp='NEP_site_mean', logx=FALSE, outfile='figures/bubble_plots/MODNPP_NEP_linear.jpeg')
bubble_plot(xvar='MOD_ann_NPP', comp='NEP_site_mean', logx=TRUE, outfile='figures/bubble_plots/MODNPP_NEP_log.jpeg')

# 9: export regression dataset ####

site_data_A = readRDS('output_data/lotic_site_info_filtered.rds') %>%
    as_tibble() %>%
    rename(sitecode = Site_ID)
site_data_B = readRDS('output_data/spatial/nhdplusv2_data.rds') %>%
    select(sitecode, lat, lon, stream_order = STREAMORDE, slope = SLOPE,
           ws_area_km2 = TOTDASQKM_corr)
site_data = full_join(site_data_A, site_data_B)

stats_set = site_data %>%
    right_join(coverage_sp) %>%
    arrange(sitecode)

write.csv(stats_set, 'export_datasets/streampulse_synthesis_statset.csv',
          row.names = FALSE)

# 10: (Figure X) interannual CVs ####

dir.create('figures/interannual_CVs', showWarnings = FALSE)

fnet_cvs = fnet_full %>%
    # mutate(NEP = GPP + ER) %>%
    group_by(sitecode, Year) %>%
    summarize(across(c(GPP, ER), sum, na.rm = TRUE)) %>%
    filter(n() > 3) %>%
    ungroup() %>%
    group_by(sitecode) %>%
    summarize(across(c(GPP, ER),
                     ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100),
              .groups = 'drop')

sp_cvs = sp_full %>%
    rename(GPP = GPP_C_filled,
           ER = ER_C_filled) %>%
    # mutate(NEP = GPP + ER) %>%
    group_by(sitecode, Year) %>%
    summarize(across(c(GPP, ER), sum, na.rm = TRUE)) %>%
    filter(n() > 3) %>%
    ungroup() %>%
    group_by(sitecode) %>%
    summarize(across(c(GPP, ER),
                     ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE) * 100),
              .groups = 'drop')

probdens2 <- function(d_sp, d_fnet, v){

    dens_sp = density(na.omit(d_sp[[v]]))
    dens_sp = tibble(x=dens_sp$x, y=dens_sp$y)
    dens_fnet = density(na.omit(d_fnet[[v]]))
    dens_fnet = tibble(x=dens_fnet$x, y=dens_fnet$y)

    xlims = range(c(range(dens_sp$x), range(dens_fnet$x)))

    plot(dens_sp$x, dens_sp$y, type='n', ann=FALSE, yaxt='n',# xaxt='n',
         bty='n', xlim=xlims)

    # tck = c(0.1, 1, 10, 100, 1000, 10000, 100000)
    # # ertck = rev(c(10000, 1000, 100, 10, 1, ))
    # tck_log = log(tck)
    # # axis(1, at=tck_log, labels=tck, cex.axis=2.4, padj=0.4)
    # # ertck_log = log(ertck) * -1
    # # axis(2, at=ertck_log, labels=ertck * -1, cex.axis=2.4, padj=0.2)
    # axis(1, at=tck_log, labels=tck, padj=-1.3, tick = TRUE, line=-0.2, tcl=-0.2)

    mtext(paste(v), 1, line=-15)
    polygon(x=c(dens_fnet$x, rev(dens_fnet$x)),
            y=c(dens_fnet$y, rep(0, nrow(dens_fnet))),
            col=alpha(fnetcolor, alpha=0.7),
            border=alpha(fnetcolor, alpha=0.7))
    polygon(x=c(dens_sp$x, rev(dens_sp$x)),
            y=c(dens_sp$y, rep(0, nrow(dens_sp))),
            col=alpha(spcolor, alpha=0.7),
            border=alpha(spcolor, alpha=0.7))
}

jpeg(width=8, height=8, units='in', res=300, quality=100, type='cairo',
     filename='figures/interannual_CVs.jpg')

par(mfrow=c(2, 1), mar=c(2,1,1,1), oma=c(0, 0, 0, 0))
probdens2(sp_cvs, fnet_cvs, 'GPP')

# jpeg(width=8, height=8, units='in', res=300, quality=100, type='cairo',
#      filename='figures/interannual_CVs/ER.jpg')

# par(mfrow=c(2, 1), mar=c(2,1,1,1), oma=c(0, 0, 0, 0))
probdens2(sp_cvs, fnet_cvs, 'ER')

dev.off()

# 11: (Figure Y) peak months ####

dir.create('figures/peak_months', showWarnings = FALSE)

library(lubridate)

error.bars <- function(x, y, upper, lower=upper, cap.length=0.1, horiz=F,...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("One or more vectors is not the same length")

    if(horiz==F) {
        arrows(x,y+upper, x, y-lower, angle=90, code=3, length=cap.length, ...)
    } else if (horiz==T) {
        arrows(x+upper,y, x-lower, y, angle=90, code=3, length=cap.length, ...)
    }
}

sitemonths_sp = sp_full %>%
    rename(GPP = GPP_C_filled,
           ER = ER_C_filled) %>%
    mutate(month = month(Date)) %>%
    group_by(sitecode, Year, month) %>%
    summarize(across(c(GPP, ER), mean, na.rm = TRUE),
              .groups = 'drop') %>%
    group_by(sitecode, month) %>%
    summarize(across(c(GPP, ER), #mean, na.rm = TRUE),
                     list(mean = ~mean(., na.rm = TRUE),
                          sd = ~sd(., na.rm = TRUE))),
              n = n(),
              .groups = 'drop')

sitemonths_fnet = fnet_full %>%
    mutate(month = month(Date)) %>%
    group_by(sitecode, Year, month) %>%
    summarize(across(c(GPP, ER), mean, na.rm = TRUE),
              .groups = 'drop') %>%
    group_by(sitecode, month) %>%
    summarize(across(c(GPP, ER), #mean, na.rm = TRUE),
                     list(mean = ~mean(., na.rm = TRUE),
                          sd = ~sd(., na.rm = TRUE))),
              n = n(),
              .groups = 'drop')

#GPP

maxmonths_etc_sp_gpp = sitemonths_sp %>%
    group_by(sitecode) %>%
    filter(GPP_mean == max(GPP_mean),
           n == max(n)) %>%
    summarize(GPP_mean = first(GPP_mean),
              GPP_sd = first(GPP_sd),
              n = first(n),
              month = first(month),
              .groups = 'drop') %>%
    mutate(SE = GPP_sd / sqrt(n))

maxmonths_etc_fnet_gpp = sitemonths_fnet %>%
    group_by(sitecode) %>%
    filter(GPP_mean == max(GPP_mean),
           n == max(n)) %>%
    summarize(GPP_mean = first(GPP_mean),
              GPP_sd = first(GPP_sd),
              n = first(n),
              month = first(month),
              .groups = 'drop') %>%
    mutate(SE = GPP_sd / sqrt(n))

maxmonths_sp_gpp = c('1' = 0, table(maxmonths_etc_sp_gpp$month), '12' = 0)
maxmonths_fnet_gpp = table(maxmonths_etc_fnet_gpp$month)
maxmonths_gpp = matrix(c(maxmonths_fnet_gpp, maxmonths_sp_gpp),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c('FLUXNET', 'StreamPULSE'),
                                   month.abb))

jpeg(width=8, height=8, units='in', res=300, quality=100, type='cairo',
     filename='figures/peak_months/GPP.jpg')

barplot(maxmonths_gpp, border = NA, beside = TRUE,
        col = c(fnetcolor, spcolor),
        ylab = 'n', main = 'Highest GPP months')
legend(x=25, y=60, legend=c('FLUXNET', 'StreamPULSE'),
       fill = c(fnetcolor, spcolor), bty = 'n', border = NA)
text(x=25, y=52, adj = 0, paste('n FLUXNET =', sum(maxmonths_sp_gpp)))
text(x=25, y=49, adj = 0, paste('n StreamPULSE =', sum(maxmonths_fnet_gpp)))

dev.off()

#ER

maxmonths_etc_sp_er = sitemonths_sp %>%
    group_by(sitecode) %>%
    filter(ER_mean == min(ER_mean),
           n == max(n)) %>%
    summarize(ER_mean = first(ER_mean),
              ER_sd = first(ER_sd),
              n = first(n),
              month = first(month),
              .groups = 'drop') %>%
    mutate(SE = ER_sd / sqrt(n))

maxmonths_etc_fnet_er = sitemonths_fnet %>%
    group_by(sitecode) %>%
    filter(ER_mean == min(ER_mean),
           n == max(n)) %>%
    summarize(ER_mean = first(ER_mean),
              ER_sd = first(ER_sd),
              n = first(n),
              month = first(month),
              .groups = 'drop') %>%
    mutate(SE = ER_sd / sqrt(n))

maxmonths_sp_er = table(maxmonths_etc_sp_er$month)
tt = table(maxmonths_etc_fnet_er$month)
maxmonths_fnet_er = c(tt[1:9], c('10' = 0), tt[10:11])
maxmonths_er = matrix(c(maxmonths_fnet_er, maxmonths_sp_er),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c('FLUXNET', 'StreamPULSE'),
                                   month.abb))

jpeg(width=8, height=8, units='in', res=300, quality=100, type='cairo',
     filename='figures/peak_months/ER.jpg')

barplot(maxmonths_er, border = NA, beside = TRUE,
        col = c(fnetcolor, spcolor),
        ylab = 'n', main = 'Highest ER months')
legend(x=25, y=55, legend=c('FLUXNET', 'StreamPULSE'),
       fill = c(fnetcolor, spcolor), bty = 'n', border = NA)
text(x=25, y=48, adj = 0, paste('n FLUXNET =', sum(maxmonths_sp_er)))
text(x=25, y=45, adj = 0, paste('n StreamPULSE =', sum(maxmonths_fnet_er)))

dev.off()
