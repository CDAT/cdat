/*
 * climatology_test.c
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "cmor.h"

/*
 * global attributes for dataset.
 */
static char *experiment_id = "historical";
static char *institution = "MIROC(AORI, NIES and JAMSTEC)";
static char *source = "MIROC4h 2009";
static char *calendar = "gregorian";
static int realization = 1;
static char *contact = "anonymous";
static char *history = NULL;
static char *comment = NULL;
static char *references = NULL;
static char *model_id = "MIROC4h";
static char *forcing = "GHG";
static int initialization_method = 1;
static int physics_version = 1;
static char *institute_id = "MIROC";
static char *parent_experiment_id = "pre-industrial control";
static char *parent_experiment_rip = "r1i1p1";
static double branch_time = 36500.;


static int
setup(void)
{
    int status;
    int message = CMOR_NORMAL;
    int action = CMOR_REPLACE_3;

    status = cmor_setup(NULL, &action, &message, NULL, NULL, NULL);
    if (status != 0) {
        fprintf(stderr, "cmor_setup() failed.\n");
        return -1;
    }

    status = cmor_dataset(
        "./",
        experiment_id,
        institution,
        source,
        calendar,
        realization,
        contact,
        history,
        comment,
        references,
        0,
        0,
        NULL,
        model_id,
        forcing,
        initialization_method,
        physics_version,
        institute_id,
        parent_experiment_id,
        &branch_time,
        parent_experiment_rip);

    if (status != 0) {
        fprintf(stderr, "cmor_dataset(): failed.\n");
        return -1;
    }
    return 0;
}


void
fill_values(float *data, size_t size1, size_t size2, double v0, double step)
{
    int n, i;

    for (n = 0; n < size2; n++)
        for (i = 0; i < size1; i++)
            *data++ = (float)(v0 + n * step);
}


int
test_convert(void)
{
#define NLON 4
#define NLAT 2
#define NPLEV 17
    static double lon[] = {0., 90., 180., 270.};
    static double lat[] = {-45., 45.};
    static double lon_bnds[] = { -45., 45, 135., 225., 315.};
    static double lat_bnds[] = { -90., 0, 90.};
    static double plevs[] = {
        1000., 925., 850., 700., 600., 500., 400., 300.,
        250., 200., 150., 100., 70., 50., 30., 20., 10.
    };
    double time[] = {
        380.5, 410., 439.5, 470., 500.5, 531.,
        561.5, 592.5, 623., 653.5, 684., 714.5
    };
    double time_bnds[] = { /* climatorogical bounds (1951-1960) */
        365., 3683., /* 1951-01-01, 1960-02-01 */
        396., 3712., /* 1951-02-01, 1960-03-01 */
        424., 3743.,
        455., 3773.,
        485., 3804.,
        516., 3834.,
        546., 3865.,
        577., 3896.,
        608., 3926.,
        638., 3957.,
        669., 3987.,
        699., 4018.  /* 1951-12-01, 1961-01-01 */
    };
    int i, axis_ids[4];
    struct {
        char *name, *units;
        double *values, *bnds;
        size_t len;
    } dims[] = {
        {"plevs",     "hPa",           plevs, NULL,     NPLEV},
        {"latitude",  "degrees_north", lat,   lat_bnds, NLAT},
        {"longitude", "degrees_east",  lon,   lon_bnds, NLON}
    };
    float miss = -999.f;
    float co2[NLON * NLAT * NPLEV];
    const int UNLIMITED = 0;
    int varid;

    assert(sizeof plevs / sizeof plevs[0] == NPLEV);
    assert(sizeof lat / sizeof lat[0] == NLAT);
    assert(sizeof lon / sizeof lon[0] == NLON);
    assert(sizeof lat_bnds / sizeof lat_bnds[0] == NLAT + 1);
    assert(sizeof lon_bnds / sizeof lon_bnds[0] == NLON + 1);

    if (cmor_axis(axis_ids, "time2", "days since 1950-1-1", UNLIMITED,
                  NULL, 'd', NULL, 0, NULL) != 0) {
        fprintf(stderr, "failed to setup time-axis.\n");
        return -1;
    }
    for (i = 0; i < 3; i++)
        if (cmor_axis(axis_ids + 1 + i,
                      dims[i].name, dims[i].units, dims[i].len,
                      dims[i].values, 'd', dims[i].bnds,
                      dims[i].bnds ? 1 : 0, NULL) != 0) {
            fprintf(stderr, "failed to setup axis(%s).\n", dims[i].name);
            return -1;
        }

    if (cmor_variable(&varid, "co2Clim", "1e-6",
                      4, axis_ids,
                      'f', &miss,
                      NULL, NULL, "CO2", NULL, NULL) != 0) {
        fprintf(stderr, "failed to setup var.\n");
        return -1;
    }

    /*
     * write a variable for each time.
     */
    for (i = 0; i < 12; i++) {
        fill_values(co2, NLON * NLAT, NPLEV, 300. + i, .01);

        if (cmor_write(varid, co2, 'f', NULL, 1,
                       time + i, time_bnds + 2 * i, NULL) != 0) {
            fprintf(stderr, "failed to write var(t=%d).\n", i);
            return -1;
        }
    }
    return 0;
}


int
main(int argc, char **argv)
{
    int table_id, rval;

    if (setup() < 0)
        exit(1);

    if (cmor_load_table("Tables/CMIP5_Amon", &table_id) != 0) {
        fprintf(stderr, "cmor_load_table() failed.\n");
        exit(1);
    }
    rval = test_convert();
    cmor_close();
    printf(rval == 0 ? "SUCCESSFUL END\n" : "ABNORMAL END\n");
    return 0;
}
