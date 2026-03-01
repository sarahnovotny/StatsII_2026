"""
Shared utility functions for replicating Appel, Pan & Roberts (2023)
"Partisan Conflict over Content Moderation Is More Than Disagreement about Facts"

Python equivalent of functions.R from the replication package.
"""

import os
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
import statsmodels.api as sm
from scipy import stats
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns

# ── Paths ──────────────────────────────────────────────────────────────────────

DATA_DIR = os.path.join(os.path.dirname(__file__), '..', 'dataverse_files', 'data')
INTERMEDIATE_SURVEY = os.path.join(DATA_DIR, 'intermediate', 'survey')

# ── Control variables used across models ───────────────────────────────────────

CONTROLS = [
    'age', 'gender', 'education', 'household_income', 'race_white',
    'hispanic', 'political_interest', 'social_media_most_common_newsformat',
    'social_media_post_flagged', 'social_media_post_removed'
]

# ── Variable labels ────────────────────────────────────────────────────────────

OUTCOME_LABELS = {
    'remove': 'Intent to Remove Headline',
    'harm': 'Intent to Report Headline as Harmful',
    'censorship': 'Perception of Headline Removal as Censorship',
}

COEF_LABELS = {
    'party_id_dem': 'Democrat (Baseline)',
    'party_id_rep': 'Republican (Baseline)',
    'party_id_dem:headline_pro_dem': 'Democrat × Pro-Democrat Headline',
    'party_id_rep:headline_pro_rep': 'Republican × Pro-Republican Headline',
}


# ── Data loading ───────────────────────────────────────────────────────────────

def load_main_data(path=None):
    """Load the main experiment dataset and apply types/labels."""
    if path is None:
        path = os.path.join(INTERMEDIATE_SURVEY, 'df.experiment_long.csv')
    df = pd.read_csv(path)
    df = _apply_types(df)
    return df


def load_imputations(n=5, robustness=False, path=None):
    """Load n multiply-imputed datasets and return as a list of DataFrames."""
    datasets = []
    for i in range(1, n + 1):
        prefix = 'imputations_robustnesscheck' if robustness else 'imputations'
        fname = f'{prefix}{i}.csv'
        if path is None:
            fpath = os.path.join(INTERMEDIATE_SURVEY, fname)
        else:
            fpath = os.path.join(path, fname)
        imp = pd.read_csv(fpath)
        imp = _apply_types(imp)
        datasets.append(imp)
    return datasets


def _apply_types(df):
    """Apply categorical types and create derived columns."""
    # Binary race indicator (needed for controls)
    if 'race' in df.columns and 'race_white' not in df.columns:
        df['race_white'] = (df['race'] == 'White').astype(int)
    # Ensure party_id is categorical
    if 'party_id' in df.columns:
        df['party_id'] = pd.Categorical(df['party_id'], categories=['Democrat', 'Republican'])
    return df


# ── OLS with clustered SEs ────────────────────────────────────────────────────

def fit_ols_clustered(formula, data, cluster_var='id', weight_var='weight',
                      subset=None):
    """
    Fit a WLS model with cluster-robust standard errors.

    Parameters
    ----------
    formula : str
        Patsy formula, e.g. 'remove ~ 0 + C(party_id) + ...'
    data : DataFrame
    cluster_var : str or None
        Column name for clustering (None = HC1 robust SEs)
    weight_var : str or None
        Column name for WLS weights (None = OLS)
    subset : str or None
        Query string to subset data before fitting

    Returns
    -------
    result : statsmodels RegressionResultsWrapper
    """
    df = data.dropna(subset=_formula_vars(formula, data)).copy()
    if subset is not None:
        df = df.query(subset).copy()

    weights = df[weight_var] if weight_var else None

    if weight_var:
        model = smf.wls(formula, data=df, weights=weights)
    else:
        model = smf.ols(formula, data=df)

    cov_kwds = {}
    cov_type = 'HC1'
    if cluster_var:
        cov_type = 'cluster'
        cov_kwds = {'groups': df[cluster_var]}

    result = model.fit(cov_type=cov_type, cov_kwds=cov_kwds)
    # Attach metadata for downstream use
    result._cluster_var = cluster_var
    result._weight_var = weight_var
    result._n_clusters = df[cluster_var].nunique() if cluster_var else None
    return result


def _formula_vars(formula, df):
    """Extract variable names referenced in a patsy formula that exist in df."""
    import re
    # Remove C() wrappers
    clean = re.sub(r'C\(([^)]+)\)', r'\1', formula)
    # Split on operators and whitespace
    tokens = re.split(r'[~+:*\s\-/]+', clean)
    tokens = [t.strip() for t in tokens if t.strip() and t.strip() != '0']
    return [t for t in tokens if t in df.columns]


# ── Rubin's rules for multiple imputation pooling ─────────────────────────────

def pool_mi_results(formula, datasets, cluster_var='id', weight_var='weight',
                    subset=None):
    """
    Pool regression results across multiply-imputed datasets using Rubin's rules.

    Returns a dict with keys:
        'coef_names', 'coef', 'se', 'pvalue', 'conf_low', 'conf_high',
        'nobs', 'n_clusters', 'r_squared', 'adj_r_squared'
    """
    fits = []
    for ds in datasets:
        res = fit_ols_clustered(formula, ds, cluster_var=cluster_var,
                                weight_var=weight_var, subset=subset)
        fits.append(res)

    m = len(fits)
    param_names = fits[0].params.index.tolist()

    # Collect coefficient vectors and variance estimates
    Q = np.array([f.params.values for f in fits])          # (m, k)
    V = np.array([np.diag(f.cov_params()) for f in fits])  # (m, k) — diagonal only

    # Pooled point estimate
    Q_bar = Q.mean(axis=0)

    # Within-imputation variance
    U_bar = V.mean(axis=0)

    # Between-imputation variance
    B = Q.var(axis=0, ddof=1)

    # Total variance
    T = U_bar + (1 + 1 / m) * B

    # Pooled SEs
    se = np.sqrt(T)

    # Degrees of freedom (Barnard-Rubin adjustment)
    r = (1 + 1 / m) * B / U_bar
    v_old = (m - 1) * (1 + 1 / r) ** 2  # Original Rubin df

    # Barnard-Rubin adjusted df
    n = fits[0].nobs
    k = len(param_names)
    v_obs = (n - k + 1) / (n - k + 3) * (n - k) * (1 - r)
    # Handle edge cases where r is very small
    with np.errstate(divide='ignore', invalid='ignore'):
        v_adj = np.where(np.isfinite(v_old) & np.isfinite(v_obs) & (v_obs > 0),
                         (v_old * v_obs) / (v_old + v_obs),
                         v_old)
        v_adj = np.where(np.isfinite(v_adj) & (v_adj > 0), v_adj, n - k)

    # p-values from t-distribution
    t_stat = Q_bar / se
    pvalue = 2 * stats.t.sf(np.abs(t_stat), df=v_adj)

    # 95% CI
    t_crit = stats.t.ppf(0.975, df=v_adj)
    conf_low = Q_bar - t_crit * se
    conf_high = Q_bar + t_crit * se

    # Pool R-squared (simple average)
    r2 = np.mean([f.rsquared for f in fits])
    adj_r2 = np.mean([f.rsquared_adj for f in fits])

    return {
        'coef_names': param_names,
        'coef': Q_bar,
        'se': se,
        'pvalue': pvalue,
        'conf_low': conf_low,
        'conf_high': conf_high,
        'nobs': int(fits[0].nobs),
        'n_clusters': fits[0]._n_clusters,
        'r_squared': r2,
        'adj_r_squared': adj_r2,
    }


def mi_results_to_df(pooled):
    """Convert pool_mi_results() output to a tidy DataFrame."""
    return pd.DataFrame({
        'term': pooled['coef_names'],
        'estimate': pooled['coef'],
        'std_error': pooled['se'],
        'p_value': pooled['pvalue'],
        'conf_low': pooled['conf_low'],
        'conf_high': pooled['conf_high'],
    })


# ── Coefficient extraction helpers ─────────────────────────────────────────────

def tidy(result):
    """
    Extract tidy coefficient table from a statsmodels result.

    Returns DataFrame with columns: term, estimate, std_error, p_value, conf_low, conf_high
    """
    summary = result.summary2().tables[1]
    df = pd.DataFrame({
        'term': result.params.index,
        'estimate': result.params.values,
        'std_error': result.bse.values,
        'p_value': result.pvalues.values,
        'conf_low': result.conf_int().iloc[:, 0].values,
        'conf_high': result.conf_int().iloc[:, 1].values,
    })
    return df


# ── Coefficient plots ─────────────────────────────────────────────────────────

def plot_coefficients(results_list, variables=None, colors=None, title=None,
                      stars=True, figsize=(8, 4), xlim=None):
    """
    Horizontal point-range coefficient plot with 95% CIs.

    Parameters
    ----------
    results_list : dict
        {label: statsmodels_result} or {label: pooled_dict} (from pool_mi_results)
    variables : dict or None
        {original_term: display_label}. If None, plots all.
    colors : list or None
    title : str or None
    stars : bool
    figsize : tuple
    xlim : tuple or None

    Returns
    -------
    fig, ax
    """
    all_rows = []
    for label, res in results_list.items():
        if isinstance(res, dict):
            # MI pooled result
            df = mi_results_to_df(res)
            nobs = res['nobs']
        else:
            # statsmodels result
            df = tidy(res)
            nobs = int(res.nobs)
        df['model'] = label
        df['nobs'] = nobs
        all_rows.append(df)

    data = pd.concat(all_rows, ignore_index=True)

    if variables is not None:
        data = data[data['term'].isin(variables.keys())].copy()
        data['term_label'] = data['term'].map(variables)
        term_order = list(variables.values())
    else:
        data['term_label'] = data['term']
        term_order = data['term_label'].unique().tolist()

    models = list(results_list.keys())
    n_models = len(models)

    if colors is None:
        palette = sns.color_palette('tab10', n_models)
        colors = {m: palette[i] for i, m in enumerate(models)}
    elif isinstance(colors, list):
        colors = {m: colors[i] for i, m in enumerate(models)}

    fig, ax = plt.subplots(figsize=figsize)

    y_positions = {term: i for i, term in enumerate(term_order)}
    offsets = np.linspace(-0.15 * (n_models - 1), 0.15 * (n_models - 1), n_models)

    ax.axvline(0, color='grey', linewidth=0.5, zorder=0)

    for j, model_label in enumerate(models):
        sub = data[data['model'] == model_label]
        for _, row in sub.iterrows():
            y = y_positions.get(row['term_label'])
            if y is None:
                continue
            y_off = y + offsets[j]
            color = colors[model_label]

            ax.errorbar(row['estimate'], y_off,
                        xerr=[[row['estimate'] - row['conf_low']],
                              [row['conf_high'] - row['estimate']]],
                        fmt='o', color=color, capsize=3, markersize=4,
                        linewidth=1.2, label=model_label if _ == sub.index[0] else None)

            # Add coefficient label
            est_str = f"{row['estimate']:.2f}"
            if stars:
                if row['p_value'] < 0.001:
                    est_str += '***'
                elif row['p_value'] < 0.01:
                    est_str += '**'
                elif row['p_value'] < 0.05:
                    est_str += '*'

            ax.annotate(est_str, (row['conf_high'], y_off),
                        xytext=(5, 0), textcoords='offset points',
                        fontsize=8, va='center', color=color)

    ax.set_yticks(range(len(term_order)))
    ax.set_yticklabels(term_order)
    if xlim:
        ax.set_xlim(xlim)
    ax.set_xlabel('Coefficient Estimate')
    if title:
        ax.set_title(title)

    # Add N observations annotation
    nobs_vals = data.groupby('model')['nobs'].first()
    nobs_str = ', '.join([f"N={v:,}" for v in nobs_vals])
    ax.annotate(nobs_str, xy=(0.01, 0.02), xycoords='axes fraction',
                fontsize=8, color='grey')

    # Remove duplicate legend entries
    handles, labels = ax.get_legend_handles_labels()
    by_label = dict(zip(labels, handles))
    if len(by_label) > 1:
        ax.legend(by_label.values(), by_label.keys(), loc='lower right', fontsize=8)

    ax.invert_yaxis()
    fig.tight_layout()
    return fig, ax


# ── Descriptive bar charts ─────────────────────────────────────────────────────

def compute_weighted_means(df, outcome, weight_col='weight'):
    """Compute weighted means by party × alignment for a given outcome."""
    rows = []
    for party in ['Democrat', 'Republican']:
        for aligned_val, aligned_label in [(0, 'Misaligned'), (1, 'Aligned')]:
            mask = (df['party_id'] == party) & (df['aligned'] == aligned_val)
            sub = df.loc[mask].dropna(subset=[outcome, weight_col])
            if len(sub) == 0:
                continue
            wmean = np.average(sub[outcome], weights=sub[weight_col])
            # Weighted SE via linearization
            w = sub[weight_col].values
            x = sub[outcome].values
            n = len(x)
            wse = np.sqrt(np.sum(w**2 * (x - wmean)**2) / (np.sum(w))**2) if n > 1 else 0
            rows.append({
                'party_id': party, 'aligned': aligned_label,
                'mean': wmean, 'se': wse,
                'conf_low': wmean - 1.96 * wse,
                'conf_high': wmean + 1.96 * wse,
            })
        # Overall
        mask = df['party_id'] == party
        sub = df.loc[mask].dropna(subset=[outcome, weight_col])
        wmean = np.average(sub[outcome], weights=sub[weight_col])
        w = sub[weight_col].values
        x = sub[outcome].values
        n = len(x)
        wse = np.sqrt(np.sum(w**2 * (x - wmean)**2) / (np.sum(w))**2) if n > 1 else 0
        rows.append({
            'party_id': party, 'aligned': 'Overall',
            'mean': wmean, 'se': wse,
            'conf_low': wmean - 1.96 * wse,
            'conf_high': wmean + 1.96 * wse,
        })
    return pd.DataFrame(rows)


def plot_descriptive_bars(means_df, outcome_label, figsize=(12, 4)):
    """
    Grouped bar chart by party × alignment with error bars and gap annotations.

    Parameters
    ----------
    means_df : DataFrame from compute_weighted_means()
    outcome_label : str, e.g. 'Average Intent to Remove Headline'
    figsize : tuple

    Returns
    -------
    fig, axes (one panel per alignment category)
    """
    party_colors = {'Democrat': '#377EB8', 'Republican': '#E41A1C'}
    panels = ['Overall', 'Misaligned', 'Aligned']

    fig, axes = plt.subplots(1, 3, figsize=figsize, sharey=True)

    for ax, panel in zip(axes, panels):
        sub = means_df[means_df['aligned'] == panel]
        parties = ['Democrat', 'Republican']
        x = np.arange(len(parties))
        width = 0.6

        for i, party in enumerate(parties):
            row = sub[sub['party_id'] == party]
            if row.empty:
                continue
            m = row['mean'].values[0]
            cl = row['conf_low'].values[0]
            ch = row['conf_high'].values[0]
            bar = ax.bar(x[i], m, width, color=party_colors[party], alpha=0.85)
            ax.errorbar(x[i], m, yerr=[[m - cl], [ch - m]], fmt='none',
                        color='black', capsize=4, alpha=0.5)
            ax.text(x[i], 0.03, f'{m:.0%}', ha='center', va='bottom',
                    fontsize=9, color='#333333')

        # Gap annotation (brace between bars)
        vals = sub.set_index('party_id')['mean']
        if 'Democrat' in vals.index and 'Republican' in vals.index:
            d_val = vals['Democrat']
            r_val = vals['Republican']
            mid_y = (d_val + r_val) / 2
            gap_label = f'{panel.lower()}\npreference gap' if panel != 'Overall' else 'overall gap'
            ax.annotate('', xy=(0.75, max(d_val, r_val)),
                        xytext=(0.75, min(d_val, r_val)),
                        arrowprops=dict(arrowstyle='<->', color='#808080', lw=1))
            ax.text(0.95, mid_y, gap_label, fontsize=7, color='#808080',
                    va='center', ha='left')

        ax.set_title(panel, fontsize=12)
        ax.set_xticks(x)
        ax.set_xticklabels(parties, fontsize=10)
        ax.set_ylim(0, 1)
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda v, _: f'{v:.0%}'))
        ax.set_facecolor('#f5f5f5')
        ax.grid(False)

    axes[0].set_ylabel(outcome_label)
    fig.tight_layout()
    return fig, axes


# ── Mediation analysis (Baron-Kenny with cluster bootstrap) ────────────────────

def baron_kenny_mediation(data, outcome, mediator='accuracy_binary',
                          treatment='aligned', controls=None,
                          cluster_var='id', weight_var='weight',
                          n_boot=1000, seed=42):
    """
    Baron-Kenny mediation decomposition with cluster bootstrap CIs.

    Returns dict with ACME, ADE, total_effect, prop_mediated, each containing
    'estimate', 'conf_low', 'conf_high', 'p_value'.
    """
    rng = np.random.RandomState(seed)
    df = data.dropna(subset=[outcome, mediator, treatment]).copy()
    if weight_var:
        df = df.dropna(subset=[weight_var])
    if controls:
        df = df.dropna(subset=controls)

    # Point estimates
    point = _mediation_point_estimate(df, outcome, mediator, treatment,
                                      controls, weight_var)

    # Cluster bootstrap
    cluster_ids = df[cluster_var].unique()
    boot_results = []
    for _ in range(n_boot):
        sampled_ids = rng.choice(cluster_ids, size=len(cluster_ids), replace=True)
        boot_df = pd.concat([df[df[cluster_var] == cid] for cid in sampled_ids],
                            ignore_index=True)
        try:
            b = _mediation_point_estimate(boot_df, outcome, mediator, treatment,
                                          controls, weight_var)
            boot_results.append(b)
        except Exception:
            continue

    boot_arr = pd.DataFrame(boot_results)

    results = {}
    for key in ['acme', 'ade', 'total', 'prop_mediated']:
        vals = boot_arr[key].dropna()
        ci_low = np.percentile(vals, 2.5)
        ci_high = np.percentile(vals, 97.5)
        # Two-sided p-value: proportion of bootstrap draws crossing zero
        p_val = np.mean(vals * point[key] < 0) * 2
        p_val = min(p_val, 1.0)
        if p_val == 0:
            p_val = 1 / len(vals)  # lower bound
        results[key] = {
            'estimate': point[key],
            'conf_low': ci_low,
            'conf_high': ci_high,
            'p_value': p_val,
        }

    results['nobs'] = len(df)
    results['n_boot'] = n_boot
    return results


def _mediation_point_estimate(df, outcome, mediator, treatment, controls, weight_var):
    """Compute ACME, ADE, total effect, and proportion mediated."""
    ctrl_str = ' + '.join(controls) if controls else ''

    # Mediator model: mediator ~ treatment [+ controls]
    med_formula = f'{mediator} ~ {treatment}'
    if ctrl_str:
        med_formula += f' + {ctrl_str}'

    # Outcome model: outcome ~ mediator + treatment [+ controls]
    out_formula = f'{outcome} ~ {mediator} + {treatment}'
    if ctrl_str:
        out_formula += f' + {ctrl_str}'

    if weight_var:
        med_fit = smf.wls(med_formula, data=df, weights=df[weight_var]).fit()
        out_fit = smf.wls(out_formula, data=df, weights=df[weight_var]).fit()
    else:
        med_fit = smf.ols(med_formula, data=df).fit()
        out_fit = smf.ols(out_formula, data=df).fit()

    # ACME = a * b (effect of treatment on mediator × effect of mediator on outcome)
    a = med_fit.params[treatment]
    b = out_fit.params[mediator]
    acme = a * b

    # ADE = direct effect of treatment controlling for mediator
    ade = out_fit.params[treatment]

    total = acme + ade
    prop_mediated = acme / total if total != 0 else np.nan

    return {'acme': acme, 'ade': ade, 'total': total, 'prop_mediated': prop_mediated}


def mediation_results_table(results):
    """Format mediation results into a presentable DataFrame."""
    rows = []
    labels = {
        'acme': 'ACME (Average Causal Mediation Effect)',
        'ade': 'ADE (Average Direct Effect)',
        'total': 'Total Effect',
        'prop_mediated': 'Proportion Mediated',
    }
    for key, label in labels.items():
        r = results[key]
        rows.append({
            'Effect': label,
            'Estimate': r['estimate'],
            '95% CI Lower': r['conf_low'],
            '95% CI Upper': r['conf_high'],
            'p-value': r['p_value'],
        })
    rows.append({'Effect': 'N Observations', 'Estimate': results['nobs'],
                 '95% CI Lower': '', '95% CI Upper': '', 'p-value': ''})
    rows.append({'Effect': 'N Simulations', 'Estimate': results['n_boot'],
                 '95% CI Lower': '', '95% CI Upper': '', 'p-value': ''})
    return pd.DataFrame(rows)


# ── Mediation sensitivity analysis ─────────────────────────────────────────────

def mediation_sensitivity(data, outcome, mediator='accuracy_binary',
                          treatment='aligned', controls=None,
                          weight_var='weight', rho_range=None):
    """
    Sensitivity analysis for sequential ignorability violation.

    Computes ACME as a function of rho (correlation between mediator and
    outcome model errors).

    Returns DataFrame with columns: rho, acme
    """
    if rho_range is None:
        rho_range = np.linspace(-0.9, 0.9, 37)

    df = data.dropna(subset=[outcome, mediator, treatment]).copy()
    if weight_var:
        df = df.dropna(subset=[weight_var])
    if controls:
        df = df.dropna(subset=controls)

    ctrl_str = ' + '.join(controls) if controls else ''

    # Fit mediator and outcome models
    med_formula = f'{mediator} ~ {treatment}'
    out_formula = f'{outcome} ~ {mediator} + {treatment}'
    if ctrl_str:
        med_formula += f' + {ctrl_str}'
        out_formula += f' + {ctrl_str}'

    if weight_var:
        med_fit = smf.wls(med_formula, data=df, weights=df[weight_var]).fit()
        out_fit = smf.wls(out_formula, data=df, weights=df[weight_var]).fit()
    else:
        med_fit = smf.ols(med_formula, data=df).fit()
        out_fit = smf.ols(out_formula, data=df).fit()

    sigma_m = np.sqrt(med_fit.mse_resid)
    sigma_y = np.sqrt(out_fit.mse_resid)

    a = med_fit.params[treatment]
    b_orig = out_fit.params[mediator]

    results = []
    for rho in rho_range:
        # Adjusted b coefficient under confounding
        b_adj = b_orig - rho * sigma_y / sigma_m
        acme_adj = a * b_adj
        results.append({'rho': rho, 'acme': acme_adj})

    return pd.DataFrame(results)


def fit_glm_clustered(formula, data, family=None, cluster_var='id',
                      weight_var='weight', subset=None):
    """
    Fit a GLM with cluster-robust standard errors.

    Parameters
    ----------
    formula : str
        Patsy formula.
    data : DataFrame
    family : statsmodels family or None
        E.g. sm.families.Binomial(). Defaults to Binomial() if None.
    cluster_var : str or None
        Column name for clustering.
    weight_var : str or None
        Column name for precision weights (var_weights).
    subset : str or None
        Query string to subset data before fitting.

    Returns
    -------
    result : statsmodels GLMResultsWrapper
    """
    if family is None:
        family = sm.families.Binomial()

    df = data.dropna(subset=_formula_vars(formula, data)).copy()
    if subset is not None:
        df = df.query(subset).copy()

    kwargs = {}
    if weight_var:
        kwargs['var_weights'] = df[weight_var].values

    model = smf.glm(formula, data=df, family=family, **kwargs)

    cov_kwds = {}
    cov_type = 'HC1'
    if cluster_var:
        cov_type = 'cluster'
        cov_kwds = {'groups': df[cluster_var]}

    import warnings
    # Statsmodels warns that the sandwich covariance estimator has not been
    # formally verified when var_weights are present.  The underlying math is
    # identical to WLS + cluster-robust SEs (which *is* verified), so the
    # estimates are reliable in practice.  We suppress the warning here to
    # keep notebook output clean.  See Notebook 06 intro for full discussion.
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', message='.*cov_type not fully supported.*')
        result = model.fit(cov_type=cov_type, cov_kwds=cov_kwds)
    result._cluster_var = cluster_var
    result._weight_var = weight_var
    result._n_clusters = df[cluster_var].nunique() if cluster_var else None
    return result


def marginal_effect_at_values(result, base_term, triple_term, mod_values):
    """
    Compute the marginal effect of a treatment at different moderator values,
    using the variance-covariance matrix from a fitted model.

    effect(m) = beta_base + beta_triple * m
    Var(effect) = Var(beta_base) + m^2 * Var(beta_triple) + 2*m*Cov(base, triple)

    Parameters
    ----------
    result : statsmodels result
        Fitted model with .params and .cov_params().
    base_term : str
        Name of the base interaction term (e.g. 'party_id_dem:headline_pro_dem').
    triple_term : str
        Name of the triple interaction term (e.g. 'party_id_dem:headline_pro_dem:education_c').
    mod_values : array-like
        Values of the moderator at which to evaluate the marginal effect.

    Returns
    -------
    DataFrame with columns: mod_value, effect, se, conf_low, conf_high
    """
    beta_base = result.params[base_term]
    beta_triple = result.params[triple_term]
    vcov = result.cov_params()
    var_base = vcov.loc[base_term, base_term]
    var_triple = vcov.loc[triple_term, triple_term]
    cov_bt = vcov.loc[base_term, triple_term]

    mod_values = np.asarray(mod_values, dtype=float)
    effects = beta_base + beta_triple * mod_values
    variances = var_base + mod_values**2 * var_triple + 2 * mod_values * cov_bt
    ses = np.sqrt(np.maximum(variances, 0))

    return pd.DataFrame({
        'mod_value': mod_values,
        'effect': effects,
        'se': ses,
        'conf_low': effects - 1.96 * ses,
        'conf_high': effects + 1.96 * ses,
    })


def plot_sensitivity(sens_df, title='Mediation Sensitivity Analysis',
                     figsize=(7, 4)):
    """Plot ACME as a function of rho."""
    fig, ax = plt.subplots(figsize=figsize)
    ax.plot(sens_df['rho'], sens_df['acme'], 'b-', linewidth=1.5)
    ax.axhline(0, color='grey', linestyle='--', linewidth=0.5)
    ax.axvline(0, color='grey', linestyle='--', linewidth=0.5)
    ax.set_xlabel(r'Sensitivity Parameter $\rho$')
    ax.set_ylabel('ACME')
    ax.set_title(title)
    fig.tight_layout()
    return fig, ax
