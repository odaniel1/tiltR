// Generated by rstantools.  Do not edit by hand.

/*
    tiltR is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    tiltR is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with tiltR.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_logistic_model_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_logistic_model");
    reader.add_event(51, 49, "end", "model_logistic_model");
    return reader;
}
#include <stan_meta_header.hpp>
class model_logistic_model
  : public stan::model::model_base_crtp<model_logistic_model> {
private:
        int N;
        std::vector<double> t;
        std::vector<double> sg;
        double og;
        double fg_ant;
        double fg_sd;
        int days;
public:
    model_logistic_model(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_logistic_model(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_logistic_model_namespace::model_logistic_model";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 3;
            context__.validate_dims("data initialization", "N", "int", context__.to_vec());
            N = int(0);
            vals_i__ = context__.vals_i("N");
            pos__ = 0;
            N = vals_i__[pos__++];
            check_greater_or_equal(function__, "N", N, 0);
            current_statement_begin__ = 4;
            validate_non_negative_index("t", "N", N);
            context__.validate_dims("data initialization", "t", "double", context__.to_vec(N));
            t = std::vector<double>(N, double(0));
            vals_r__ = context__.vals_r("t");
            pos__ = 0;
            size_t t_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < t_k_0_max__; ++k_0__) {
                t[k_0__] = vals_r__[pos__++];
            }
            size_t t_i_0_max__ = N;
            for (size_t i_0__ = 0; i_0__ < t_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "t[i_0__]", t[i_0__], 0);
            }
            current_statement_begin__ = 5;
            validate_non_negative_index("sg", "N", N);
            context__.validate_dims("data initialization", "sg", "double", context__.to_vec(N));
            sg = std::vector<double>(N, double(0));
            vals_r__ = context__.vals_r("sg");
            pos__ = 0;
            size_t sg_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < sg_k_0_max__; ++k_0__) {
                sg[k_0__] = vals_r__[pos__++];
            }
            size_t sg_i_0_max__ = N;
            for (size_t i_0__ = 0; i_0__ < sg_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "sg[i_0__]", sg[i_0__], 1);
            }
            current_statement_begin__ = 8;
            context__.validate_dims("data initialization", "og", "double", context__.to_vec());
            og = double(0);
            vals_r__ = context__.vals_r("og");
            pos__ = 0;
            og = vals_r__[pos__++];
            check_greater_or_equal(function__, "og", og, 1);
            current_statement_begin__ = 11;
            context__.validate_dims("data initialization", "fg_ant", "double", context__.to_vec());
            fg_ant = double(0);
            vals_r__ = context__.vals_r("fg_ant");
            pos__ = 0;
            fg_ant = vals_r__[pos__++];
            check_greater_or_equal(function__, "fg_ant", fg_ant, 1);
            current_statement_begin__ = 12;
            context__.validate_dims("data initialization", "fg_sd", "double", context__.to_vec());
            fg_sd = double(0);
            vals_r__ = context__.vals_r("fg_sd");
            pos__ = 0;
            fg_sd = vals_r__[pos__++];
            check_greater_or_equal(function__, "fg_sd", fg_sd, 0);
            current_statement_begin__ = 15;
            context__.validate_dims("data initialization", "days", "int", context__.to_vec());
            days = int(0);
            vals_i__ = context__.vals_i("days");
            pos__ = 0;
            days = vals_i__[pos__++];
            check_greater_or_equal(function__, "days", days, 0);
            // initialize transformed data variables
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 19;
            num_params_r__ += 1;
            current_statement_begin__ = 20;
            num_params_r__ += 1;
            current_statement_begin__ = 21;
            num_params_r__ += 1;
            current_statement_begin__ = 22;
            num_params_r__ += 1;
            current_statement_begin__ = 23;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_logistic_model() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 19;
        if (!(context__.contains_r("b")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable b missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("b");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "b", "double", context__.to_vec());
        double b(0);
        b = vals_r__[pos__++];
        try {
            writer__.scalar_ub_unconstrain(0, b);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable b: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 20;
        if (!(context__.contains_r("M")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable M missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("M");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "M", "double", context__.to_vec());
        double M(0);
        M = vals_r__[pos__++];
        try {
            writer__.scalar_unconstrain(M);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable M: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 21;
        if (!(context__.contains_r("fg")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable fg missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("fg");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "fg", "double", context__.to_vec());
        double fg(0);
        fg = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(1, fg);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable fg: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 22;
        if (!(context__.contains_r("nu")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable nu missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("nu");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "nu", "double", context__.to_vec());
        double nu(0);
        nu = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, nu);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable nu: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 23;
        if (!(context__.contains_r("sigma")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma", "double", context__.to_vec());
        double sigma(0);
        sigma = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sigma);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 19;
            local_scalar_t__ b;
            (void) b;  // dummy to suppress unused var warning
            if (jacobian__)
                b = in__.scalar_ub_constrain(0, lp__);
            else
                b = in__.scalar_ub_constrain(0);
            current_statement_begin__ = 20;
            local_scalar_t__ M;
            (void) M;  // dummy to suppress unused var warning
            if (jacobian__)
                M = in__.scalar_constrain(lp__);
            else
                M = in__.scalar_constrain();
            current_statement_begin__ = 21;
            local_scalar_t__ fg;
            (void) fg;  // dummy to suppress unused var warning
            if (jacobian__)
                fg = in__.scalar_lb_constrain(1, lp__);
            else
                fg = in__.scalar_lb_constrain(1);
            current_statement_begin__ = 22;
            local_scalar_t__ nu;
            (void) nu;  // dummy to suppress unused var warning
            if (jacobian__)
                nu = in__.scalar_lb_constrain(0, lp__);
            else
                nu = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 23;
            local_scalar_t__ sigma;
            (void) sigma;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma = in__.scalar_lb_constrain(0, lp__);
            else
                sigma = in__.scalar_lb_constrain(0);
            // transformed parameters
            current_statement_begin__ = 27;
            validate_non_negative_index("f", "N", N);
            std::vector<local_scalar_t__> f(N, local_scalar_t__(0));
            stan::math::initialize(f, DUMMY_VAR__);
            stan::math::fill(f, DUMMY_VAR__);
            // transformed parameters block statements
            current_statement_begin__ = 28;
            for (int n = 1; n <= N; ++n) {
                current_statement_begin__ = 29;
                stan::model::assign(f, 
                            stan::model::cons_list(stan::model::index_uni(n), stan::model::nil_index_list()), 
                            (fg + ((og - fg) / pow((1 + stan::math::exp((-(b) * (get_base1(t, n, "t", 1) - M)))), (1 / nu)))), 
                            "assigning variable f");
            }
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 27;
            size_t f_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < f_k_0_max__; ++k_0__) {
                if (stan::math::is_uninitialized(f[k_0__])) {
                    std::stringstream msg__;
                    msg__ << "Undefined transformed parameter: f" << "[" << k_0__ << "]";
                    stan::lang::rethrow_located(std::runtime_error(std::string("Error initializing variable f: ") + msg__.str()), current_statement_begin__, prog_reader__());
                }
            }
            // model body
            current_statement_begin__ = 34;
            lp_accum__.add(normal_log<propto__>(fg, fg_ant, fg_sd));
            current_statement_begin__ = 35;
            lp_accum__.add(normal_log<propto__>(b, 0, 2));
            current_statement_begin__ = 36;
            lp_accum__.add(exponential_log<propto__>(M, 1));
            current_statement_begin__ = 37;
            lp_accum__.add(exponential_log<propto__>(nu, 1));
            current_statement_begin__ = 38;
            lp_accum__.add(normal_log<propto__>(sigma, 0, 0.0005));
            current_statement_begin__ = 41;
            lp_accum__.add(normal_log<propto__>(sg, f, sigma));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("b");
        names__.push_back("M");
        names__.push_back("fg");
        names__.push_back("nu");
        names__.push_back("sigma");
        names__.push_back("f");
        names__.push_back("sg_fit");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(N);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back((1 + (48 * days)));
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_logistic_model_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        double b = in__.scalar_ub_constrain(0);
        vars__.push_back(b);
        double M = in__.scalar_constrain();
        vars__.push_back(M);
        double fg = in__.scalar_lb_constrain(1);
        vars__.push_back(fg);
        double nu = in__.scalar_lb_constrain(0);
        vars__.push_back(nu);
        double sigma = in__.scalar_lb_constrain(0);
        vars__.push_back(sigma);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            // declare and define transformed parameters
            current_statement_begin__ = 27;
            validate_non_negative_index("f", "N", N);
            std::vector<double> f(N, double(0));
            stan::math::initialize(f, DUMMY_VAR__);
            stan::math::fill(f, DUMMY_VAR__);
            // do transformed parameters statements
            current_statement_begin__ = 28;
            for (int n = 1; n <= N; ++n) {
                current_statement_begin__ = 29;
                stan::model::assign(f, 
                            stan::model::cons_list(stan::model::index_uni(n), stan::model::nil_index_list()), 
                            (fg + ((og - fg) / pow((1 + stan::math::exp((-(b) * (get_base1(t, n, "t", 1) - M)))), (1 / nu)))), 
                            "assigning variable f");
            }
            if (!include_gqs__ && !include_tparams__) return;
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            // write transformed parameters
            if (include_tparams__) {
                size_t f_k_0_max__ = N;
                for (size_t k_0__ = 0; k_0__ < f_k_0_max__; ++k_0__) {
                    vars__.push_back(f[k_0__]);
                }
            }
            if (!include_gqs__) return;
            // declare and define generated quantities
            current_statement_begin__ = 45;
            validate_non_negative_index("sg_fit", "(1 + (48 * days))", (1 + (48 * days)));
            std::vector<double> sg_fit((1 + (48 * days)), double(0));
            stan::math::initialize(sg_fit, DUMMY_VAR__);
            stan::math::fill(sg_fit, DUMMY_VAR__);
            // generated quantities statements
            current_statement_begin__ = 47;
            for (int s = 1; s <= (1 + (48 * days)); ++s) {
                current_statement_begin__ = 48;
                stan::model::assign(sg_fit, 
                            stan::model::cons_list(stan::model::index_uni(s), stan::model::nil_index_list()), 
                            ((normal_rng(0, sigma, base_rng__) + fg) + ((og - fg) / pow((1 + stan::math::exp((-(b) * (((s - 1) / 48.0) - M)))), (1 / nu)))), 
                            "assigning variable sg_fit");
            }
            // validate, write generated quantities
            current_statement_begin__ = 45;
            size_t sg_fit_k_0_max__ = (1 + (48 * days));
            for (size_t k_0__ = 0; k_0__ < sg_fit_k_0_max__; ++k_0__) {
                vars__.push_back(sg_fit[k_0__]);
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_logistic_model";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "b";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "M";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "fg";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "nu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t f_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < f_k_0_max__; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "f" << '.' << k_0__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
        size_t sg_fit_k_0_max__ = (1 + (48 * days));
        for (size_t k_0__ = 0; k_0__ < sg_fit_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "sg_fit" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "b";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "M";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "fg";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "nu";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t f_k_0_max__ = N;
            for (size_t k_0__ = 0; k_0__ < f_k_0_max__; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "f" << '.' << k_0__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
        size_t sg_fit_k_0_max__ = (1 + (48 * days));
        for (size_t k_0__ = 0; k_0__ < sg_fit_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "sg_fit" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
    }
}; // model
}  // namespace
typedef model_logistic_model_namespace::model_logistic_model stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif
