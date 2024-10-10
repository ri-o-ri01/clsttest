ENUM_PLP_TYPE <- new.env()
ENUM_PLP_TYPE$SIMPLE = list(name = "simple", 
                         use_algo = FALSE, algo_name = NULL, is_mix = FALSE,
                         use_cluster = FALSE, use_synthe_bs = NULL,
                         plp_types = c("simple")
                         )
ENUM_PLP_TYPE$CLUST_INV = list(name = "clust_inv", 
                            use_algo = FALSE, algo_name = NULL, is_mix = FALSE,
                            use_cluster = TRUE, use_synthe_bs = NULL,
                            plp_types = c("clust_inv")
                            )
ENUM_PLP_TYPE$TSD    = list(name = "tsd", 
                         use_algo = TRUE, algo_name = "tsd", is_mix = FALSE,
                         use_cluster = FALSE, use_synthe_bs = FALSE,
                         plp_types = c("tsd")
                         )
ENUM_PLP_TYPE$SYNTHE_BS_TSD = list(name = "synthe_bs_tsd",
                                use_algo = TRUE, algo_name = "tsd", is_mix = FALSE,
                                use_cluster = FALSE, use_synthe_bs = TRUE,
                                plp_types = c("synthe_bs_tsd")
)

ENUM_PLP_TYPE$MIX_TSD       = list(name = "mix_tsd",
                                use_algo = TRUE, algo_name = "tsd", is_mix = TRUE,
                                use_cluster = FALSE, use_synthe_bs = NULL,
                                plp_types = c("tsd", "synthe_bs_tsd")
                                )
