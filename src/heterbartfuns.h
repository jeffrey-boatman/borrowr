/*
 *  combr: combine data sources to estimate population average treatment effect.
 *  Copyright (C) 2019  Jeffrey A. Verdoliva Boatman
 *  This code is a modified version from the BART R package from April 2019.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.

 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.

 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef GUARD_heterbartfuns_h
#define GUARD_heterbartfuns_h

#include "tree.h"
#include "treefuns.h"
#include "info.h"

//--------------------------------------------------
//heterlh, replacement for lil that only depends on sum y.
// theirs
// double heterlh(double b, double M, double tau);
//mine
double heterlh(double b, double M, double sigma, double tau);
//--------------------------------------------------
//compute b and M  for left and right give bot and v,c
void hetergetsuff(tree& x, tree::tree_p nx, size_t v, size_t c, xinfo& xi, dinfo& di, size_t& nl, double& bl, double& Ml, size_t& nr, double& br, double& Mr, double *sigma);
//--------------------------------------------------
//compute b and M for left and right bots
void hetergetsuff(tree& x, tree::tree_p l, tree::tree_p r, xinfo& xi, dinfo& di, double& bl, double& Ml, double& br, double& Mr, double *sigma);
//--------------------------------------------------
//draw one mu from post
// double heterdrawnodemu(double b, double M, double tau, rn& gen);
double heterdrawnodemu(double b, double M, double sigma, double gamma, rn& gen);
//--------------------------------------------------
//get sufficients stats for all bottom nodes, this way just loop through all the data once.
void heterallsuff(tree& x, xinfo& xi, dinfo& di, tree::npv& bnv, std::vector<double>& bv, std::vector<double>& Mv, double *sigma);
//--------------------------------------------------
//heter version of drmu, need b and M instead of n and sy
void heterdrmu(tree& t, xinfo& xi, dinfo& di, pinfo& pi, double *sigma, rn& gen);

#endif
