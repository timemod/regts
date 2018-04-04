#include "intpol_cspline.h"
#ifdef DEBUG
#include <stdio.h>
#endif

static int csplin(int n, double x[], double y[], 
                  const char conds, double *work[4]);
static int csrand(int n, double x[], double y[], 
                   const char conds, double b[4]);
static int csybar(int n, int nbar, double x[], double *work[4],
                  double xbar[], double ybar[]);

int intpol_cspline(int n, int nnew, double x[], double y[],
                   double xnew[], double ynew[], char conds,
                   double *work[4]) {

    /* 
     * Interpolation by cubic spline interpolation.
     * Arguments:
     *      n      input     number of data points x and y.
     *      x, y   input     coordinates of given points.
     *      xnew   input     x coordinates of the points to be interpolated
     *      ynew   output    intepolation results.
     *      conds  input     a character specifying the type of boundary
     *                       conditions: 'n' for natural spline,
     *                       '3' for not-a-knot-spline (third derivative
     *                       continious at x[1] and x[n-1].
     *      work   work      work array, a 2D array with 4 rows 
     *                       and at least n columns.
     *
     * Return value retval:
     *     0   ok
     *    -1   no data points available (n == 0)
     *    -2   illegal boundary condition conds
     *   > 0   xbar[retval - 1] not in the range between x[0] and x[n - 1]
     */   
    if (n == 0) {
        return -1;
    } 

    int ierr = csplin(n - 1, x, y, conds, work);
    if (ierr != 0) return ierr;

#ifdef DEBUG
    printf("After csplin, ierr = %d\n", ierr);
    int i, j;
    printf("coefficients:\n");
    for (i = 0; i < n; i++) {
        for (j = 0; j < 4; j++) {
            printf("%10g", work[j][i]);
        }
        printf("\n");
    }
#endif

    ierr = csybar(n - 1, nnew, x, work, xnew, ynew);

    return ierr;
}


static int csplin(int n, double x[], double y[],
              const char conds, double *c[4]) {

    /*
    Calculate derivatives using the cubic spline method
    making the first and second derivative continuous.
    n is the number of intervals. The number of points in x and y is n + 1.

    For i = 1 .. n-1  we have
 
    (x[i+1] - x[i]) *f'[i-1] + 2*(x[i+1]-x[i-1])*f'[i] + (x[i]-x[i-1])*f'[i-1]
     = 3*(f(x[i-1],x[i])*(x[i+1]-x[i])+f(x[i],x[i+1])*(x[i]-x[i-1]))

    Boundary conditions:
         f'(x[0]) + b[0]*f'(x[1]) = b[2]
         f'(x[n]) + b[2]*f'(x[n-1]) = b[4]

    The coefficients b are termined in function csrand.
    Thus we have  n-1 continuous connections and two boundary conditions.
    
    Return value is -1 is conds in an invalid boundary condition

    */

    double b[4], f, dxr, dd1, dd3;
    int i, ii;

    for (i = 0; i < n + 1; i++) {
        c[0][i] = y[i];
    }

    if (n == 0) {
        // single point, all derivatives zero.
        for (i = 1; i < 4; i++) {
            for (int j = 0; j < n + 1; j++) {
                c[i][j] = 0.0;
            }
        }
        return 0;
    }


    // boundary conditions
    int ier = csrand(n, x, y, conds, b);
    if (ier != 0) return ier;

#ifdef DEBUG
    printf(" b coefficients csrand:\n");
    for (i = 0; i < 4; i++) {
        printf("%g\n", b[i]);
    }
    printf("\n");
#endif

    /* for the time being, the right hand side is stored in c[1],
     * the diagonal in c[2] and the subdiagonal in c[3] */

    c[2][0] = 1;
    c[3][0] = b[0];
    for (i = 0; i < n; i++) {
        c[3][i + 1] = x[i + 1] - x[i];
        c[2][i + 1] = (c[0][i + 1] - c[0][i]) / c[3][i + 1];
    }
    
    c[1][0] = b[1];
    for (i = 1; i < n; i++) {
        c[1][i] = 3 * (c[3][i] * c[2][i + 1] + c[3][i + 1] * c[2][i]);
        c[2][i] = 2 * (c[3][i] + c[3][i + 1]);
    }
    c[1][n] = b[3];

 
    // start elimination of the tridiagonal (c[2] /c[3]), also convert the 
    // right hand side of the matrix equation (c[1])

    for (i = 1; i < n; i++) {
        f = c[3][i + 1] / c[2][i - 1];
        c[2][i] = c[2][i] - f * c[3][i - 1];
        c[1][i] = c[1][i] - f * c[1][i - 1];
    }

    f = b[2] / c[2][n - 1];
    c[1][n] = (c[1][n] -f * c[1][n - 1]) / (1.0 - f * c[3][n - 1]);

    // back substitution
    for (ii = 2; ii <= n; ii++) {
        i = n + 1 - ii;
        c[1][i] = (c[1][i] - c[3][i] * c[1][i + 1])/ c[2][i];
    }
    c[1][0] = c[1][0] - c[3][0] * c[1][1];

    // Calculate the coefficients for piecewise cubic interpolation
    // given the function values and derivatives.

    for (i = 0; i < n; i++) {
        dxr = 1.0 / (x[i + 1] - x[i]);
        // dd1=first divided difference = f(x(i),x(i+1)) = df/dx
        dd1 = (c[0][i + 1] - c[0][i]) * dxr;
        // dd3=third divided difference = f(x(i),x(i),x(i+1),x(i+1)) *dx*dx
        dd3 = c[1][i] + c[1][i + 1] - 2 * dd1;
        c[2][i] = (dd1 - c[1][i] - dd3) * dxr;
        c[3][i] = dd3 *dxr * dxr;
    }

    return 0;
}
 
static int csrand(int n, double x[], double y[], 
                  char conds, double b[4]) {

    /* Determine coefficients b for the boundary conditions.
     * conds specifies the type of boundary condition.
     *      '3'   f'''(x[1]) and f'''(x[n-1]) continuous
     *      'n'   natural spline
     *
     * n is the number of intervals.
     * The number of points in x and y is n + 1.
     *
     * The return value is -1 is conds in an invalid boundary condition
     */

    double xx1213;
    int  i, i1, i2, i3;

    if (n == 1) {
        /* A single interval. In that case use the natural spline,
         * which in this case will generate a linear interpolation.
         */
        conds = 'n';
    }

  
    // fdd = first divided difference
    #define fdd(i, j) ((y[i] - y[j]) / (x[i] - x[j]))
    #define xdx(i, j, k, l) ((x[i] - x[j])/ (x[k] - x[l]))
    
    for (i = 0; i < 3; i+= 2) {

        if (i == 0) {
            // left side points
            i1 = 0;
            i2 = 1;
            i3 = 2;
        } else if (i == 2) {
            // right side points
            i1 = n;
            i2 = n - 1;
            i3 = n - 2;
        }

        switch (conds) {

            case '3': b[i] = xdx(i1, i3, i2, i3);
                      xx1213 = xdx(i1, i2, i1, i3);
                      b[i + 1] = fdd(i1, i2) * (2 + xx1213) + fdd(i2, i3) * 
                                 xx1213 * xdx(i1, i2, i2, i3);
                      break;

            case 'n': b[i] = 0.5;
                      b[i + 1] = 1.5 * fdd(i1, i2);
                      break;

            default: return -1;
        }
    }

    return 0;
}



static int csybar(int n, int nbar, double x[], double **c,
                  double xbar[], double ybar[]) {

    /* Calculate new y values corresponding to new x values, based on
     * the coefficients of the partial third order polynomials.
     *
     * ybar = c[1](i) + c[1][i] * d + c[2][i] * d**2 + c[3][i] * d**3
     *  d = xbar - x[i]
     *  x[i] < xbar < x[i + 1]
    
     *  n = number of intervals in x 
     *  nbar= number of points in xbar (input) and ybar (output)

     *  The values in xbar do not have to be in increasing order,
     *  however this function is more efficient if xbar is orderd.
     *
     *  Return code retval:
     *      0    ok
     *    > 0    xbar[retval -1] is not in the range between x[0] and x[n]
     *           (extrapolation is not implemented)
     */
     
    int i_start, n1, i, l;
    double dx;
    
    // i_start is the index in array x where we start looking
    i_start = 0;

    n1 = n + 1; // number of x points

    bool found;
    
    for (l = 0; l < nbar; l++) {
    
        found = false;

        //  search forwards
        for (i = i_start; i < n1; i++) {
            dx = xbar[l] - x[i];
            if (i == i_start && dx < 0) break;
            found = dx <= 0;
            if (found) {
                if (dx < 0) {
                    i = i - 1;
                    dx = xbar[l] - x[i];
                }
                break;
            }
        }

        if (!found) {
            // search backwards
            if (i_start == 0) break;
            for (i = i_start - 1; i >= 0; i--) {
                dx = xbar[l] - x[i];
                if (dx >= 0) {
                    found = true;
                    break;
                }
            }
        }  
         
        if (found) {
            // compute ybar in the interval
            ybar[l] = c[0][i];
            if (dx > 0) ybar[l] = ybar[l] + dx *(c[1][i] + dx * 
                                   (c[2][i] + dx * c[3][i]));
            i_start = i;

        } else {

           break;

        }

    }
    
    if (found) {
        return 0;
    } else {
        return l;
    }
}

