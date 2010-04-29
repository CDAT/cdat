/* First-order conservative regrid */

void conserv_regrid (int num_links, int nextra, int ninput, int noutput, double input[], double output[], double remap_matrix[], int src_address[], int dst_address[]) {

    /* double input[nextra][ninput],
       double output[nextra][noutput],
       double remap_matrix[num_links][3],
       src_address[num_links],
       dst_address[num_links]
       */

    int n, k, daddr, saddr;

    /*    printf("num_links = %d\n", num_links);
    printf("nextra = %d\n", nextra);
    printf("ninput = %d\n", ninput);
    printf("noutput = %d\n", noutput);
    */

    for (k=0; k<nextra; k++){
	for (n=0; n<num_links; n++){
	    daddr = dst_address[n]-1;
	    saddr = src_address[n]-1;
	    output[noutput*k+daddr] = output[noutput*k+daddr] + remap_matrix[3*n]*input[ninput*k+saddr];
	}
    }

}

/* First-order conservative regrid, with normalization.  'normal'
   should be 1.0 for normalize_opt=='fracarea', grid_frac for
   normalize_opt=='destarea', or grid_frac*grid_area for
   normalize_opt=='none' */

void conserv_regrid_normal (int num_links, int nextra, int ninput, int noutput, double input[], double output[], double remap_matrix[], int src_address[], int dst_address[], double normal[]) {

    /* double input[nextra][ninput],
       double output[nextra][noutput],
       double remap_matrix[num_links][3],
       int src_address[num_links],
       int dst_address[num_links],
       double normal[noutput]
       */

    int n, k, daddr, saddr;

    for (k=0; k<nextra; k++){
	for (n=0; n<num_links; n++){
	    daddr = dst_address[n]-1;
	    saddr = src_address[n]-1;
	    output[noutput*k+daddr] = output[noutput*k+daddr]
		+ (remap_matrix[3*n]*input[ninput*k+saddr]) / (normal[daddr]);
	}
    }

}


/* Second-order conservative regrid.
   src_grad1 is the latitude gradient df/dtheta
   src_grad2 is the longitude gradient (1/cos(theta))(df/dphi) */

void conserv_regrid2 (int num_links, int nextra, int ninput, int noutput, double input[], double output[], double remap_matrix[], int src_address[], int dst_address[], double src_grad1[], double src_grad2[]) {

    /* double input[nextra][ninput],
       double output[nextra][noutput],
       double remap_matrix[num_links][3],
       src_address[num_links],
       dst_address[num_links],
       double src_grad1[nextra][ninput],
       double src_grad2[nextra][ninput]
       */

    int n, k, daddr, saddr;

    for (k=0; k<nextra; k++){
	for (n=0; n<num_links; n++){
	    daddr = dst_address[n]-1;
	    saddr = src_address[n]-1;
	    output[noutput*k+daddr] = output[noutput*k+daddr]
		+ remap_matrix[3*n]*input[ninput*k+saddr]
		+ src_grad1[ninput*k+saddr]*remap_matrix[3*n+1]
		+ src_grad2[ninput*k+saddr]*remap_matrix[3*n+2];
	}
    }
}

/* Bilinear regrid */

void bilinear_regrid (int num_links, int nextra, int ninput, int noutput, double input[], double output[], double remap_matrix[], int src_address[], int dst_address[]) {

    /* double input[nextra][ninput],
       double output[nextra][noutput],
       double remap_matrix[num_links][1],
       src_address[num_links],
       dst_address[num_links]
       */

    int n, k, daddr, saddr;

    for (k=0; k<nextra; k++){
	for (n=0; n<num_links; n++){
	    daddr = dst_address[n]-1;
	    saddr = src_address[n]-1;
	    output[noutput*k+daddr] = output[noutput*k+daddr] + remap_matrix[n]*input[ninput*k+saddr];
	}
    }

}

/* Bicubic regrid */

void bicubic_regrid (int num_links, int nextra, int ninput, int noutput, double input[], double output[], double remap_matrix[], int src_address[], int dst_address[], double src_grad1[], double src_grad2[], double src_grad3[]) {

    /* double input[nextra][ninput],
       double output[nextra][noutput],
       double remap_matrix[num_links][4],
       src_address[num_links],
       dst_address[num_links],
       double src_grad1[nextra][ninput],
       double src_grad2[nextra][ninput],
       double src_grad3[nextra][ninput]
       */

    int n, k, daddr, saddr;

    for (k=0; k<nextra; k++){
	for (n=0; n<num_links; n++){
	    daddr = dst_address[n]-1;
	    saddr = src_address[n]-1;
	    output[noutput*k+daddr] = output[noutput*k+daddr]
		+ remap_matrix[4*n]*input[ninput*k+saddr]
		+ src_grad1[ninput*k+saddr]*remap_matrix[4*n+1]
		+ src_grad2[ninput*k+saddr]*remap_matrix[4*n+2]
		+ src_grad3[ninput*k+saddr]*remap_matrix[4*n+3];
	}
    }
}

/* Distance-weighted regrid */

void distwgt_regrid (int num_links, int nextra, int ninput, int noutput, double input[], double output[], double remap_matrix[], int src_address[], int dst_address[]) {


    /* double input[nextra][ninput],
       double output[nextra][noutput],
       double remap_matrix[num_links][1],
       src_address[num_links],
       dst_address[num_links]
       */

    int n, k, daddr, saddr;

    for (k=0; k<nextra; k++){
	for (n=0; n<num_links; n++){
	    daddr = dst_address[n]-1;
	    saddr = src_address[n]-1;
	    output[noutput*k+daddr] = output[noutput*k+daddr] + remap_matrix[n]*input[ninput*k+saddr];
	}
    }

}

