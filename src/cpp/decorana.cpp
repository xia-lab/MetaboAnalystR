#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

using Rcpp::IntegerVector;
using Rcpp::List;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;

namespace {

struct CEPData {
  int mi;
  int n;
  int nid;
  std::vector<int> ibegin;
  std::vector<int> iend;
  std::vector<int> idat;
  std::vector<double> qidat;
};

CEPData data2hill_cpp(const NumericMatrix& veg) {
  const int nr = veg.nrow();
  const int nc = veg.ncol();

  if (nr <= 0 || nc <= 0) {
    Rcpp::stop("zero extent dimensions");
  }

  CEPData out;
  out.mi = nr;
  out.n = nc;
  out.ibegin.resize(nr);
  out.iend.resize(nr);

  int now = 0;
  for (int i = 0; i < nr; ++i) {
    out.ibegin[i] = now;
    for (int j = 0; j < nc; ++j) {
      const double v = veg(i, j);
      if (v > 0.0) {
        out.idat.push_back(j);
        out.qidat.push_back(v);
        ++now;
      }
    }
    out.iend[i] = now;
  }

  out.nid = now;
  return out;
}

void rwarn(const std::string& msg) {
  Rcpp::warning(msg);
}

void xmaxmi(const std::vector<double>& x, double& axmax, double& axmin) {
  axmax = -1.0e10;
  axmin = 1.0e10;
  for (double ax : x) {
    if (ax > axmax) {
      axmax = ax;
    }
    if (ax < axmin) {
      axmin = ax;
    }
  }
}

void xymult(const std::vector<double>& x,
            std::vector<double>& y,
            int mi,
            int n,
            const std::vector<int>& ibegin,
            const std::vector<int>& iend,
            const std::vector<int>& idat,
            const std::vector<double>& qidat) {
  std::fill(y.begin(), y.end(), 0.0);
  for (int i = 0; i < mi; ++i) {
    const int id1 = ibegin[i];
    const int id2 = iend[i];
    const double ax = x[i];
    for (int id = id1; id < id2; ++id) {
      const int j = idat[id];
      y[j] += ax * qidat[id];
    }
  }
}

void yxmult(const std::vector<double>& y,
            std::vector<double>& x,
            int mi,
            const std::vector<int>& ibegin,
            const std::vector<int>& iend,
            const std::vector<int>& idat,
            const std::vector<double>& qidat) {
  for (int i = 0; i < mi; ++i) {
    const int id1 = ibegin[i];
    const int id2 = iend[i];
    double ax = 0.0;
    for (int id = id1; id < id2; ++id) {
      const int j = idat[id];
      ax += y[j] * qidat[id];
    }
    x[i] = ax;
  }
}

void cutup(const std::vector<double>& x, std::vector<int>& ix, int mi, int mk) {
  const int mmk = mk - 4;
  const int maxk = mk - 2;
  double axmax;
  double axmin;
  xmaxmi(x, axmax, axmin);
  double axbit = (axmax - axmin) / static_cast<double>(mmk);
  if (axbit == 0.0) {
    axbit = 1.0;
  }

  for (int i = 0; i < mi; ++i) {
    int iax = static_cast<int>((x[i] - axmin) / axbit) + 3;
    if (iax < 3) {
      iax = 3;
    }
    if (iax > maxk) {
      iax = maxk;
    }
    ix[i] = iax;
  }
}

void detrnd(std::vector<double>& x,
            const std::vector<double>& aidot,
            const std::vector<int>& ix,
            int mi,
            int mk) {
  std::vector<double> z(50, 0.0);
  std::vector<double> zn(50, 0.0);
  std::vector<double> zbar(50, 0.0);

  for (int i = 0; i < mi; ++i) {
    const int k = ix[i] - 1;
    z[k] += x[i] * aidot[i];
    zn[k] += aidot[i];
  }

  int mmk = mk - 1;
  for (int k = 1; k < mmk; ++k) {
    zbar[k] = (z[k - 1] + z[k] + z[k + 1]) /
              (zn[k - 1] + zn[k] + zn[k + 1] + 1.0e-12);
  }

  mmk = mmk - 1;
  for (int k = 2; k < mmk; ++k) {
    z[k] = (zbar[k - 1] + zbar[k] + zbar[k + 1]) / 3.0;
  }

  for (int i = 0; i < mi; ++i) {
    const int k = ix[i] - 1;
    x[i] -= z[k];
  }
}

void trans(const std::vector<double>& y,
           std::vector<double>& yy,
           std::vector<double>& x,
           int neig,
           int ira,
           const std::vector<double>& aidot,
           const std::vector<double>& xeig1,
           const std::vector<double>& xeig2,
           const std::vector<double>& xeig3,
           const std::vector<int>& ix1,
           const std::vector<int>& ix2,
           const std::vector<int>& ix3,
           int mi,
           int mk,
           int n,
           const std::vector<int>& ibegin,
           const std::vector<int>& iend,
           const std::vector<int>& idat,
           const std::vector<double>& qidat) {
  yxmult(y, x, mi, ibegin, iend, idat, qidat);
  for (int i = 0; i < mi; ++i) {
    x[i] /= aidot[i];
  }

  if (neig != 0) {
    if (ira != 1) {
      detrnd(x, aidot, ix1, mi, mk);
      if (neig > 1) {
        detrnd(x, aidot, ix2, mi, mk);
      }
      if (neig > 2) {
        detrnd(x, aidot, ix3, mi, mk);
        detrnd(x, aidot, ix2, mi, mk);
      }
      if (neig > 1) {
        detrnd(x, aidot, ix1, mi, mk);
      }
    } else {
      double a1 = 0.0;
      for (int i = 0; i < mi; ++i) {
        a1 += aidot[i] * x[i] * xeig1[i];
      }
      for (int i = 0; i < mi; ++i) {
        x[i] -= a1 * xeig1[i];
      }

      if (neig > 1) {
        double a2 = 0.0;
        for (int i = 0; i < mi; ++i) {
          a2 += aidot[i] * x[i] * xeig2[i];
        }
        for (int i = 0; i < mi; ++i) {
          x[i] -= a2 * xeig2[i];
        }
      }

      if (neig > 2) {
        double a3 = 0.0;
        for (int i = 0; i < mi; ++i) {
          a3 += aidot[i] * x[i] * xeig3[i];
        }
        for (int i = 0; i < mi; ++i) {
          x[i] -= a3 * xeig3[i];
        }
      }
    }
  }

  xymult(x, yy, mi, n, ibegin, iend, idat, qidat);
}

void smooth(std::vector<double>& z, int mk) {
  int istop = 1;
  for (int icount = 1; icount <= 50; ++icount) {
    double az2 = z[0];
    double az3 = z[1];
    if (az3 == 0.0) {
      istop = 0;
    }
    z[0] = 0.75 * az2 + 0.25 * az3;

    for (int k3 = 2; k3 < mk; ++k3) {
      double az1 = az2;
      az2 = az3;
      az3 = z[k3];
      if (az3 <= 0.0) {
        istop = 0;
      }
      z[k3 - 1] = 0.5 * (az2 + 0.5 * (az1 + az3));
    }

    z[mk - 1] = 0.25 * az2 + 0.75 * az3;
    istop = istop + 1;
    if (istop == 4) {
      return;
    }
  }
}

void segmnt(std::vector<double>& x,
            std::vector<double>& y,
            std::vector<double>& zn,
            std::vector<double>& zv,
            int mi,
            int mk,
            const std::vector<double>& aidot,
            const std::vector<int>& ibegin,
            const std::vector<int>& iend,
            const std::vector<int>& idat,
            const std::vector<double>& qidat) {
  for (int k = 0; k < mk; ++k) {
    zn[k] = -1.0e-20;
    zv[k] = -1.0e-20;
  }

  double axmax;
  double axmin;
  xmaxmi(x, axmax, axmin);
  double axbit = (axmax - axmin) / static_cast<double>(mk);
  if (axbit == 0.0) {
    axbit = 1.0;
  }

  for (int i = 0; i < mi; ++i) {
    x[i] -= axmin;
  }
  const int n = static_cast<int>(y.size());
  for (int j = 0; j < n; ++j) {
    y[j] -= axmin;
  }

  for (int i = 0; i < mi; ++i) {
    double sqcorr = 0.0;
    double sumsq = 2.0e-20;
    const int id1 = ibegin[i];
    const int id2 = iend[i];
    const double ax = x[i];

    for (int id = id1; id < id2; ++id) {
      const int j = idat[id];
      const double aij = qidat[id];
      sqcorr += aij * aij;
      const double d = ax - y[j];
      sumsq += aij * d * d;
    }

    sqcorr /= (aidot[i] * aidot[i]);
    if (sqcorr > 0.9999) {
      sqcorr = 0.9999;
    }
    sumsq /= aidot[i];

    int k = static_cast<int>(ax / axbit) + 1;
    if (k > mk) {
      k = mk;
    }
    if (k < 1) {
      k = 1;
    }

    zv[k - 1] += sumsq;
    zn[k - 1] += 1.0 - sqcorr;
  }
}

void strtch(std::vector<double>& x,
            std::vector<double>& y,
            double shortv,
            int monit,
            int mi,
            int n,
            const std::vector<double>& aidot,
            const std::vector<int>& ibegin,
            const std::vector<int>& iend,
            const std::vector<int>& idat,
            const std::vector<double>& qidat) {
  monit = 0;
  std::vector<double> zn(51, 0.0);
  std::vector<double> zv(51, 0.0);

  for (int icount = 1; icount <= 2; ++icount) {
    int mk = 20;
    segmnt(x, y, zn, zv, mi, mk, aidot, ibegin, iend, idat, qidat);
    smooth(zv, mk);
    smooth(zn, mk);

    double zvsum = 0.0;
    for (int k = 0; k < mk; ++k) {
      zvsum += zv[k] / zn[k];
    }

    double sd = std::sqrt(zvsum / static_cast<double>(mk));
    double along = 0.0;
    for (int i = 0; i < mi; ++i) {
      double ax = x[i] / sd;
      x[i] = ax;
      if (along < ax) {
        along = ax;
      }
    }

    for (int j = 0; j < n; ++j) {
      y[j] /= sd;
    }

    if (along < shortv) {
      return;
    }
    if (icount == 2) {
      return;
    }

    mk = static_cast<int>(along * 5.0) + 1;
    if (mk < 10) {
      mk = 10;
    }
    if (mk > 45) {
      mk = 45;
    }

    segmnt(x, y, zn, zv, mi, mk, aidot, ibegin, iend, idat, qidat);
    smooth(zv, mk);
    smooth(zn, mk);

    zvsum = 0.0;
    for (int k = 0; k < mk; ++k) {
      const double azv = 1.0 / std::sqrt(0.2 / along + zv[k] / zn[k]);
      zvsum += azv;
      zv[k] = azv;
    }

    for (int k = 0; k < mk; ++k) {
      zv[k] = zv[k] * along / zvsum;
    }

    double az = 0.0;
    zn[0] = 0.0;
    for (int k = 0; k < mk; ++k) {
      az += zv[k];
      zn[k + 1] = az;
    }

    const double axbit = along / static_cast<double>(mk);
    for (int j = 0; j < n; ++j) {
      int iay = static_cast<int>(y[j] / axbit) + 1;
      if (iay < 1) {
        iay = 1;
      }
      if (iay > mk) {
        iay = mk;
      }
      y[j] = zn[iay - 1] + zv[iay - 1] *
              (y[j] / axbit - static_cast<double>(iay - 1));
    }

    yxmult(y, x, mi, ibegin, iend, idat, qidat);
    for (int i = 0; i < mi; ++i) {
      x[i] /= aidot[i];
    }
  }
}

void eigy(std::vector<double>& x,
          std::vector<double>& y,
          double& eig,
          int neig,
          int ira,
          int iresc,
          double shortv,
          int mi,
          int mk,
          int n,
          const std::vector<int>& ibegin,
          const std::vector<int>& iend,
          const std::vector<int>& idat,
          const std::vector<double>& qidat,
          std::vector<double>& y2,
          std::vector<double>& y3,
          std::vector<double>& y4,
          std::vector<double>& y5,
          const std::vector<double>& xeig1,
          const std::vector<double>& xeig2,
          const std::vector<double>& xeig3,
          const std::vector<int>& ix1,
          const std::vector<int>& ix2,
          const std::vector<int>& ix3,
          const std::vector<double>& aidot,
          const std::vector<double>& adotj) {
  double tot = 0.0;
  for (int j = 0; j < n; ++j) {
    tot += adotj[j];
    y[j] = static_cast<double>(j + 1);
  }
  y[0] = 1.1;

  const double tol = 0.000005;
  trans(y, y, x, neig, ira, aidot, xeig1, xeig2, xeig3,
        ix1, ix2, ix3, mi, mk, n, ibegin, iend, idat, qidat);

  int icount = 0;
  double a11 = 0.0;
  double a12 = 0.0;

  for (;;) {
    double a = 0.0;
    for (int j = 0; j < n; ++j) {
      a += y[j] * adotj[j];
    }
    a /= tot;

    double ex = 0.0;
    for (int j = 0; j < n; ++j) {
      const double ay = y[j] - a;
      ex += ay * ay * adotj[j];
      y[j] = ay;
    }

    ex = std::sqrt(ex);
    for (int j = 0; j < n; ++j) {
      y[j] /= ex;
    }

    trans(y, y2, x, neig, ira, aidot, xeig1, xeig2, xeig3,
          ix1, ix2, ix3, mi, mk, n, ibegin, iend, idat, qidat);

    a = 0.0;
    a11 = 0.0;
    a12 = 0.0;
    double a22 = 0.0;
    double a23 = 0.0;
    double a33 = 0.0;
    double a34 = 0.0;
    double a44 = 0.0;

    for (int j = 0; j < n; ++j) {
      const double ay = y2[j];
      y2[j] = ay / adotj[j];
      a += ay;
      a11 += ay * y[j];
    }
    a /= tot;

    for (int j = 0; j < n; ++j) {
      const double ay = y2[j] - (a + a11 * y[j]);
      a12 += ay * ay * adotj[j];
      y2[j] = ay;
    }

    a12 = std::sqrt(a12);
    for (int j = 0; j < n; ++j) {
      y2[j] /= a12;
    }

    if (a12 < tol) {
      break;
    }
    if (icount > 999) {
      break;
    }

    ++icount;

    trans(y2, y3, x, neig, ira, aidot, xeig1, xeig2, xeig3,
          ix1, ix2, ix3, mi, mk, n, ibegin, iend, idat, qidat);

    a = 0.0;
    double b13 = 0.0;
    for (int j = 0; j < n; ++j) {
      const double ay = y3[j];
      y3[j] = ay / adotj[j];
      a += ay;
      a22 += ay * y2[j];
      b13 += ay * y[j];
    }
    a /= tot;

    for (int j = 0; j < n; ++j) {
      const double ay = y3[j] - (a + a22 * y2[j] + b13 * y[j]);
      a23 += ay * ay * adotj[j];
      y3[j] = ay;
    }

    a23 = std::sqrt(a23);
    if (a23 > tol) {
      for (int j = 0; j < n; ++j) {
        y3[j] /= a23;
      }

      trans(y3, y4, x, neig, ira, aidot, xeig1, xeig2, xeig3,
            ix1, ix2, ix3, mi, mk, n, ibegin, iend, idat, qidat);

      a = 0.0;
      double b14 = 0.0;
      double b24 = 0.0;
      for (int j = 0; j < n; ++j) {
        const double ay = y4[j];
        y4[j] = y4[j] / adotj[j];
        a += ay;
        a33 += ay * y3[j];
        b14 += ay * y[j];
        b24 += ay * y2[j];
      }
      a /= tot;

      for (int j = 0; j < n; ++j) {
        const double ay = y4[j] - (a + a33 * y3[j] + b14 * y[j] + b24 * y2[j]);
        a34 += ay * ay * adotj[j];
        y4[j] = ay;
      }

      a34 = std::sqrt(a34);
      if (a34 > tol) {
        for (int j = 0; j < n; ++j) {
          y4[j] /= a34;
        }

        trans(y4, y5, x, neig, ira, aidot, xeig1, xeig2, xeig3,
              ix1, ix2, ix3, mi, mk, n, ibegin, iend, idat, qidat);
        for (int j = 0; j < n; ++j) {
          a44 += y4[j] * y5[j];
        }
      } else {
        a34 = 0.0;
      }
    } else {
      a23 = 0.0;
    }

    double ax1 = 1.0;
    double ax2 = 0.1;
    double ax3 = 0.01;
    double ax4 = 0.001;

    for (int itimes = 1; itimes <= 100; ++itimes) {
      const double axx1 = a11 * ax1 + a12 * ax2;
      const double axx2 = a12 * ax1 + a22 * ax2 + a23 * ax3;
      const double axx3 = a23 * ax2 + a33 * ax3 + a34 * ax4;
      const double axx4 = a34 * ax3 + a44 * ax4;

      ax1 = a11 * axx1 + a12 * axx2;
      ax2 = a12 * axx1 + a22 * axx2 + a23 * axx3;
      ax3 = a23 * axx2 + a33 * axx3 + a34 * axx4;
      ax4 = a34 * axx3 + a44 * axx4;

      const double ex2 = std::sqrt(ax1 * ax1 + ax2 * ax2 + ax3 * ax3 + ax4 * ax4);
      ax1 /= ex2;
      ax2 /= ex2;
      ax3 /= ex2;
      ax4 /= ex2;

      if (itimes != (itimes / 5) * 5) {
        continue;
      }

      const double exx = std::sqrt(ex2);
      const double resi = std::sqrt((ax1 - axx1 / exx) * (ax1 - axx1 / exx) +
                                    (ax2 - axx2 / exx) * (ax2 - axx2 / exx) +
                                    (ax3 - axx3 / exx) * (ax3 - axx3 / exx) +
                                    (ax4 - axx4 / exx) * (ax4 - axx4 / exx));
      if (resi < tol * 0.05) {
        break;
      }
    }

    for (int j = 0; j < n; ++j) {
      y[j] = ax1 * y[j] + ax2 * y2[j] + ax3 * y3[j] + ax4 * y4[j];
    }
  }

  if (a12 > tol) {
    std::string axnam = "4";
    if (neig == 0) axnam = "1";
    if (neig == 1) axnam = "2";
    if (neig == 2) axnam = "3";
    rwarn("residual bigger than tolerance on axis " + axnam);
  }

  double aymax;
  double aymin;
  xmaxmi(y, aymax, aymin);
  double sign = 1.0;
  if (-aymin > aymax) {
    sign = -1.0;
  }

  for (int j = 0; j < n; ++j) {
    y[j] *= sign;
  }

  yxmult(y, x, mi, ibegin, iend, idat, qidat);
  for (int i = 0; i < mi; ++i) {
    x[i] /= aidot[i];
  }

  if (iresc != 0 && a11 <= 0.999) {
    for (int i = 1; i <= iresc; ++i) {
      int monit = 0;
      if (i == 1 || i == iresc) {
        monit = 1;
      }
      strtch(x, y, shortv, monit, mi, n, aidot, ibegin, iend, idat, qidat);
    }
    eig = a11;
    return;
  }

  double axlong = 0.0;
  for (int i = 0; i < mi; ++i) {
    axlong += aidot[i] * x[i] * x[i];
  }
  axlong = std::sqrt(axlong);

  for (int i = 0; i < mi; ++i) {
    x[i] /= axlong;
  }
  for (int j = 0; j < n; ++j) {
    y[j] /= axlong;
  }

  double sumsq = 0.0;
  for (int i = 0; i < mi; ++i) {
    const int id1 = ibegin[i];
    const int id2 = iend[i];
    const double ax = x[i];
    for (int id = id1; id < id2; ++id) {
      const int j = idat[id];
      const double d = ax - y[j];
      sumsq += qidat[id] * d * d;
    }
  }

  double sd = std::sqrt(sumsq / tot);
  if (a11 > 0.999) {
    sd = aymax / axlong;
    const double sd1 = -aymin / axlong;
    if (sd1 > sd) {
      sd = sd1;
    }
  }

  for (int j = 0; j < n; ++j) {
    y[j] /= sd;
  }

  eig = a11;
}

std::vector<double> get_col(const NumericMatrix& m, int col) {
  const int nr = m.nrow();
  std::vector<double> out(nr);
  for (int i = 0; i < nr; ++i) {
    out[i] = m(i, col);
  }
  return out;
}

void set_col(NumericMatrix& m, int col, const std::vector<double>& v) {
  const int nr = m.nrow();
  for (int i = 0; i < nr; ++i) {
    m(i, col) = v[i];
  }
}

} // namespace

// [[Rcpp::export]]
List do_decorana_cpp(NumericMatrix veg,
                     int ira,
                     int iresc,
                     double rshort,
                     int imk,
                     NumericVector aidot,
                     NumericVector adotj) {
  const int NAXES = 4;
  const int MKPAD = 4;
  const double ZEROEIG = 1e-7;

  const CEPData cep = data2hill_cpp(veg);
  const int nr = cep.mi;
  const int nc = cep.n;
  const int mk = imk + MKPAD;

  if (mk > 50) {
    Rcpp::stop("mk + 4 exceeds translated decorana segment limit (50)");
  }
  if (aidot.size() != nr) {
    Rcpp::stop("length of aidot must equal nrow(veg)");
  }
  if (adotj.size() != nc) {
    Rcpp::stop("length of adotj must equal ncol(veg)");
  }

  std::vector<double> aidotv(aidot.begin(), aidot.end());
  std::vector<double> adotjv(adotj.begin(), adotj.end());

  NumericMatrix xeig(nr, NAXES);
  NumericMatrix yeig(nc, NAXES);
  NumericVector evals(NAXES);

  std::vector<int> ix1(nr, 3);
  std::vector<int> ix2(nr, 3);
  std::vector<int> ix3(nr, 3);

  std::vector<double> y2(nc, 0.0);
  std::vector<double> y3(nc, 0.0);
  std::vector<double> y4(nc, 0.0);
  std::vector<double> y5(nc, 0.0);

  for (int i = 0; i < NAXES; ++i) {
    std::vector<double> x(nr, 0.0);
    std::vector<double> y(nc, 0.0);
    std::vector<double> xe1 = get_col(xeig, 0);
    std::vector<double> xe2 = get_col(xeig, 1);
    std::vector<double> xe3 = get_col(xeig, 2);

    double eig = 0.0;
    eigy(x, y, eig, i, ira, iresc, rshort, nr, mk, nc,
         cep.ibegin, cep.iend, cep.idat, cep.qidat,
         y2, y3, y4, y5,
         xe1, xe2, xe3,
         ix1, ix2, ix3,
         aidotv, adotjv);

    if (eig < ZEROEIG) {
      std::fill(x.begin(), x.end(), 0.0);
      std::fill(y.begin(), y.end(), 0.0);
      eig = 0.0;
    }

    set_col(xeig, i, x);
    set_col(yeig, i, y);
    evals[i] = eig;

    if (!ira && i != NAXES - 1) {
      if (i == 0) {
        cutup(x, ix1, nr, mk);
      } else if (i == 1) {
        cutup(x, ix2, nr, mk);
      } else if (i == 2) {
        cutup(x, ix3, nr, mk);
      }
    }
  }

  for (int i = 0; i < NAXES; ++i) {
    std::vector<double> y = get_col(yeig, i);
    std::vector<double> x(nr, 0.0);
    yxmult(y, x, nr, cep.ibegin, cep.iend, cep.idat, cep.qidat);
    for (int j = 0; j < nr; ++j) {
      x[j] /= aidotv[j];
    }
    set_col(xeig, i, x);
  }

  List out = List::create(
    Rcpp::Named("evals") = evals,
    Rcpp::Named("rproj") = xeig,
    Rcpp::Named("cproj") = yeig,
    Rcpp::Named("adotj") = adotj,
    Rcpp::Named("aidot") = aidot,
    Rcpp::Named("ira") = ira,
    Rcpp::Named("iresc") = iresc,
    Rcpp::Named("short") = rshort,
    Rcpp::Named("mk") = imk
  );
  out.attr("class") = "decorana";

  return out;
}
