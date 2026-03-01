import { useState } from "react";

const colors = {
  framework: "#8B0000",
  frameworkLight: "#FEF2F2",
  normal: "#1E40AF",
  normalLight: "#EFF6FF",
  bernoulli: "#047857",
  bernoulliLight: "#ECFDF5",
  poisson: "#B45309",
  poissonLight: "#FFFBEB",
  mle: "#6B21A8",
  mleLight: "#FAF5FF",
  gray: "#6B7280",
  grayLight: "#F9FAFB",
};

const DetailPanel = ({ title, content, color, onClose }) => (
  <div
    style={{
      background: "white",
      border: `2px solid ${color}`,
      borderRadius: 8,
      padding: 16,
      marginTop: 16,
      position: "relative",
    }}
  >
    <button
      onClick={onClose}
      style={{
        position: "absolute",
        top: 8,
        right: 12,
        background: "none",
        border: "none",
        fontSize: 18,
        cursor: "pointer",
        color: colors.gray,
      }}
    >
      ✕
    </button>
    <div style={{ fontWeight: 700, color, marginBottom: 8, fontSize: 14 }}>
      {title}
    </div>
    <div style={{ fontSize: 13, lineHeight: 1.6, color: "#374151" }}>
      {content}
    </div>
  </div>
);

const Box = ({ label, sub, color, bg, onClick, small, width }) => (
  <div
    onClick={onClick}
    style={{
      background: bg || "white",
      border: `2px solid ${color}`,
      borderRadius: 8,
      padding: small ? "6px 10px" : "10px 14px",
      cursor: onClick ? "pointer" : "default",
      textAlign: "center",
      width: width || "auto",
      transition: "box-shadow 0.2s",
      boxShadow: onClick ? `0 1px 3px rgba(0,0,0,0.1)` : "none",
    }}
    onMouseEnter={(e) => {
      if (onClick) e.currentTarget.style.boxShadow = `0 2px 8px rgba(0,0,0,0.15)`;
    }}
    onMouseLeave={(e) => {
      e.currentTarget.style.boxShadow = onClick ? `0 1px 3px rgba(0,0,0,0.1)` : "none";
    }}
  >
    <div
      style={{
        fontWeight: 700,
        fontSize: small ? 12 : 14,
        color,
      }}
    >
      {label}
    </div>
    {sub && (
      <div style={{ fontSize: small ? 11 : 12, color: colors.gray, marginTop: 2 }}>
        {sub}
      </div>
    )}
  </div>
);

const Arrow = ({ label, vertical, color, dashed }) => (
  <div
    style={{
      display: "flex",
      flexDirection: vertical ? "column" : "row",
      alignItems: "center",
      gap: 2,
      color: color || colors.gray,
    }}
  >
    {vertical ? (
      <>
        <div
          style={{
            width: 2,
            height: 20,
            background: color || colors.gray,
            borderStyle: dashed ? "dashed" : "solid",
          }}
        />
        <div style={{ fontSize: 10, fontStyle: "italic", textAlign: "center" }}>
          {label}
        </div>
        <div style={{ fontSize: 16 }}>↓</div>
      </>
    ) : (
      <>
        <div style={{ fontSize: 10, fontStyle: "italic" }}>{label}</div>
        <div style={{ fontSize: 16 }}>→</div>
      </>
    )}
  </div>
);

const details = {
  glm: {
    title: "Generalized Linear Model",
    color: colors.framework,
    content: (
      <div>
        <p>
          A GLM extends OLS to non-normal outcomes through three components.
          The general form is: <b>g(E[Y|X]) = Xβ</b>
        </p>
        <p>
          OLS is the special case where all three components take their simplest
          form: Normal distribution, identity link, linear predictor.
        </p>
      </div>
    ),
  },
  random: {
    title: "Random Component — Distribution of Y",
    color: colors.framework,
    content: (
      <div>
        <p>
          The assumed probability distribution for the response variable. Must
          be from the <b>exponential family</b>.
        </p>
        <p>
          This is the most crucial and subjective modelling choice — it
          determines the likelihood function and everything downstream.
        </p>
      </div>
    ),
  },
  link: {
    title: "Link Function — g(μ)",
    color: colors.framework,
    content: (
      <div>
        <p>
          Maps E[Y] from its restricted range to (−∞, +∞) so it can equal the
          linear predictor. The <b>canonical link</b> is the one where the
          natural parameter θ from EFF equals Xβ directly.
        </p>
        <p>
          You can choose non-canonical links (e.g., probit instead of logit for
          binary data), but the canonical link simplifies estimation.
        </p>
      </div>
    ),
  },
  systematic: {
    title: "Systematic Component — Linear Predictor",
    color: colors.framework,
    content: (
      <div>
        <p>
          <b>η = Xβ</b> — the linear combination of covariates and
          coefficients. Lives on (−∞, +∞). This part is the same across all
          GLMs.
        </p>
        <p>
          X is the n×k design matrix, β is the k×1 coefficient vector. Each
          observation gets its own η_i.
        </p>
      </div>
    ),
  },
  eff: {
    title: "Exponential Family Form",
    color: colors.framework,
    content: (
      <div>
        <p>
          <b>f(y|θ) = exp[yθ − b(θ) + c(y)]</b>
        </p>
        <p>
          The canonical form that unifies all supported distributions. θ is the
          natural parameter, b(θ) is the normalising constant (gives moments via
          differentiation), c(y) depends only on data.
        </p>
        <p>
          Guarantees: concave log-likelihood, unique MLE, sufficient statistics,
          moments from b(θ).
        </p>
      </div>
    ),
  },
  btheta: {
    title: "b(θ) — Normalising Constant",
    color: colors.framework,
    content: (
      <div>
        <p>
          The key function in EFF. Derivatives give moments:
        </p>
        <p>
          <b>E[Y] = b′(θ)</b> — first derivative gives the mean
          <br />
          <b>Var(Y) = b″(θ)</b> — second derivative gives the variance
        </p>
        <p>
          Normal: θ²/2 → mean = θ, var = 1<br />
          Bernoulli: ln(1+eᶿ) → mean = π, var = π(1−π)<br />
          Poisson: eᶿ → mean = μ, var = μ
        </p>
      </div>
    ),
  },
  normal: {
    title: "Normal / OLS (special case)",
    color: colors.normal,
    content: (
      <div>
        <p>
          <b>Distribution:</b> Y ~ N(μ, σ²)<br />
          <b>Natural parameter:</b> θ = μ<br />
          <b>b(θ):</b> θ²/2<br />
          <b>Canonical link:</b> Identity, g(μ) = μ<br />
          <b>Mean:</b> μ &nbsp; <b>Variance:</b> σ² (free parameter)
        </p>
        <p>
          <b>Interpretation:</b> 1-unit increase in X → β change in E[Y]
          <br />
          Closed-form MLE: β̂ = (X′X)⁻¹X′Y. No Newton-Raphson needed.
        </p>
      </div>
    ),
  },
  bernoulli: {
    title: "Bernoulli / Logistic Regression",
    color: colors.bernoulli,
    content: (
      <div>
        <p>
          <b>Distribution:</b> Y ~ Bernoulli(π)<br />
          <b>Natural parameter:</b> θ = ln(π/(1−π)) [log-odds]<br />
          <b>b(θ):</b> ln(1+eᶿ)<br />
          <b>Canonical link:</b> Logit, g(π) = ln(π/(1−π))<br />
          <b>Mean:</b> π &nbsp; <b>Variance:</b> π(1−π) (determined by mean)
        </p>
        <p>
          <b>Interpretation:</b> 1-unit increase in X → β change in log-odds;
          exp(β) = odds ratio
          <br />
          No closed-form MLE — requires Newton-Raphson.
        </p>
      </div>
    ),
  },
  poisson: {
    title: "Poisson / Count Regression",
    color: colors.poisson,
    content: (
      <div>
        <p>
          <b>Distribution:</b> Y ~ Poisson(μ)<br />
          <b>Natural parameter:</b> θ = ln(μ)<br />
          <b>b(θ):</b> eᶿ<br />
          <b>Canonical link:</b> Log, g(μ) = ln(μ)<br />
          <b>Mean:</b> μ &nbsp; <b>Variance:</b> μ (mean = variance)
        </p>
        <p>
          <b>Interpretation:</b> 1-unit increase in X → multiply E[Y] by exp(β)
          <br />
          No closed-form MLE — requires Newton-Raphson. Mean = variance is a strong
          assumption; check for overdispersion.
        </p>
      </div>
    ),
  },
  mle: {
    title: "MLE Estimation Machinery",
    color: colors.mle,
    content: (
      <div>
        <p>
          <b>Likelihood:</b> L(θ|y) = ∏ f(yᵢ|θ) — probability of data given
          parameters
          <br />
          <b>Log-likelihood:</b> ℓ = Σ ln f(yᵢ|θ) — sums instead of products
          <br />
          <b>Score:</b> u(θ) = ∂ℓ/∂θ — set to zero at MLE
          <br />
          <b>Hessian:</b> ∂²ℓ/∂θ² — curvature of log-likelihood
          <br />
          <b>Fisher info:</b> I(θ) = −E[Hessian] — its inverse gives Var(θ̂)
        </p>
      </div>
    ),
  },
  nr: {
    title: "Newton-Raphson",
    color: colors.mle,
    content: (
      <div>
        <p>
          Iterative algorithm for finding the MLE when no closed-form solution
          exists:
        </p>
        <p>
          <b>β⁽ʲ⁺¹⁾ = β⁽ʲ⁾ − [score] × [Hessian]⁻¹</b>
        </p>
        <p>
          Score gives direction (which way to step), Hessian gives step size
          (how far). Iterate until score ≈ 0. Converges reliably because EFF
          guarantees concavity.
        </p>
      </div>
    ),
  },
  odds: {
    title: "Probability ↔ Odds ↔ Log-Odds",
    color: colors.bernoulli,
    content: (
      <div>
        <p>
          Three scales for the same quantity:
        </p>
        <p>
          <b>Probability π</b> ∈ (0, 1)<br />
          → <b>Odds</b> = π/(1−π) ∈ (0, ∞)<br />
          → <b>Log-odds</b> = ln(π/(1−π)) ∈ (−∞, +∞)
        </p>
        <p>
          Logistic regression coefficients are on the log-odds scale.
          Exponentiate to get odds ratios: OR = exp(β).
        </p>
      </div>
    ),
  },
};

export default function GLMMap() {
  const [selected, setSelected] = useState(null);

  const toggle = (key) => setSelected(selected === key ? null : key);

  return (
    <div
      style={{
        fontFamily: "system-ui, -apple-system, sans-serif",
        maxWidth: 780,
        margin: "0 auto",
        padding: 20,
        background: "#FAFAFA",
        minHeight: "100vh",
      }}
    >
      <div
        style={{
          textAlign: "center",
          marginBottom: 20,
        }}
      >
        <h2
          style={{
            color: colors.framework,
            margin: 0,
            fontSize: 20,
          }}
        >
          GLM Concept Map
        </h2>
        <p style={{ color: colors.gray, fontSize: 12, margin: "4px 0 0" }}>
          Click any box for details
        </p>
      </div>

      {/* GLM Framework */}
      <div
        style={{
          background: colors.frameworkLight,
          border: `2px solid ${colors.framework}`,
          borderRadius: 12,
          padding: 16,
          marginBottom: 4,
        }}
      >
        <div style={{ textAlign: "center", marginBottom: 12 }}>
          <Box
            label="GENERALIZED LINEAR MODEL"
            sub="g(E[Y|X]) = η = Xβ"
            color={colors.framework}
            bg="white"
            onClick={() => toggle("glm")}
          />
        </div>

        {/* Three components */}
        <div
          style={{
            display: "flex",
            justifyContent: "center",
            gap: 12,
            flexWrap: "wrap",
          }}
        >
          <Box
            label="① Random Component"
            sub="Distribution of Y (from EFF)"
            color={colors.framework}
            bg="white"
            onClick={() => toggle("random")}
            width={200}
          />
          <Box
            label="② Link Function"
            sub="g(μ) = η — maps E[Y] to linear predictor"
            color={colors.framework}
            bg="white"
            onClick={() => toggle("link")}
            width={220}
          />
          <Box
            label="③ Systematic Component"
            sub="η = Xβ — linear predictor"
            color={colors.framework}
            bg="white"
            onClick={() => toggle("systematic")}
            width={200}
          />
        </div>

        {selected && ["glm", "random", "link", "systematic"].includes(selected) && (
          <DetailPanel {...details[selected]} onClose={() => setSelected(null)} />
        )}
      </div>

      {/* EFF and b(theta) */}
      <div style={{ display: "flex", justifyContent: "center" }}>
        <Arrow vertical label="distributions must be in" color={colors.framework} />
      </div>

      <div
        style={{
          background: colors.grayLight,
          border: `2px solid ${colors.gray}`,
          borderRadius: 12,
          padding: 16,
          marginBottom: 4,
        }}
      >
        <div
          style={{
            display: "flex",
            justifyContent: "center",
            gap: 16,
            flexWrap: "wrap",
          }}
        >
          <Box
            label="Exponential Family Form"
            sub="f(y|θ) = exp[yθ − b(θ) + c(y)]"
            color="#374151"
            bg="white"
            onClick={() => toggle("eff")}
            width={260}
          />
          <Box
            label="b(θ) — Normalising Constant"
            sub="b′(θ) = E[Y] &nbsp;&nbsp; b″(θ) = Var(Y)"
            color="#374151"
            bg="white"
            onClick={() => toggle("btheta")}
            width={260}
          />
        </div>
        {selected && ["eff", "btheta"].includes(selected) && (
          <DetailPanel {...details[selected]} onClose={() => setSelected(null)} />
        )}
      </div>

      {/* Arrow to specific distributions */}
      <div style={{ display: "flex", justifyContent: "center" }}>
        <Arrow vertical label="specific instances" color={colors.gray} />
      </div>

      {/* Three specific distributions */}
      <div
        style={{
          display: "flex",
          justifyContent: "center",
          gap: 12,
          marginBottom: 4,
          flexWrap: "wrap",
        }}
      >
        {/* Normal */}
        <div
          style={{
            background: colors.normalLight,
            border: `2px solid ${colors.normal}`,
            borderRadius: 12,
            padding: 12,
            width: 220,
            cursor: "pointer",
          }}
          onClick={() => toggle("normal")}
        >
          <div
            style={{
              fontWeight: 700,
              color: colors.normal,
              fontSize: 14,
              textAlign: "center",
            }}
          >
            Normal
          </div>
          <div style={{ fontSize: 11, color: "#374151", marginTop: 6 }}>
            <div>θ = μ</div>
            <div>b(θ) = θ²/2</div>
            <div>Link: Identity</div>
            <div>g(μ) = μ</div>
            <div style={{ marginTop: 4, fontStyle: "italic", color: colors.gray }}>
              = OLS (special case)
            </div>
          </div>
        </div>

        {/* Bernoulli */}
        <div
          style={{
            background: colors.bernoulliLight,
            border: `2px solid ${colors.bernoulli}`,
            borderRadius: 12,
            padding: 12,
            width: 220,
            cursor: "pointer",
          }}
          onClick={() => toggle("bernoulli")}
        >
          <div
            style={{
              fontWeight: 700,
              color: colors.bernoulli,
              fontSize: 14,
              textAlign: "center",
            }}
          >
            Bernoulli
          </div>
          <div style={{ fontSize: 11, color: "#374151", marginTop: 6 }}>
            <div>θ = ln(π/(1−π))</div>
            <div>b(θ) = ln(1+eᶿ)</div>
            <div>Link: Logit</div>
            <div>g(π) = ln(π/(1−π))</div>
            <div style={{ marginTop: 4, fontStyle: "italic", color: colors.gray }}>
              → Logistic regression
            </div>
          </div>
        </div>

        {/* Poisson */}
        <div
          style={{
            background: colors.poissonLight,
            border: `2px solid ${colors.poisson}`,
            borderRadius: 12,
            padding: 12,
            width: 220,
            cursor: "pointer",
          }}
          onClick={() => toggle("poisson")}
        >
          <div
            style={{
              fontWeight: 700,
              color: colors.poisson,
              fontSize: 14,
              textAlign: "center",
            }}
          >
            Poisson
          </div>
          <div style={{ fontSize: 11, color: "#374151", marginTop: 6 }}>
            <div>θ = ln(μ)</div>
            <div>b(θ) = eᶿ</div>
            <div>Link: Log</div>
            <div>g(μ) = ln(μ)</div>
            <div style={{ marginTop: 4, fontStyle: "italic", color: colors.gray }}>
              → Count regression
            </div>
          </div>
        </div>
      </div>

      {selected && ["normal", "bernoulli", "poisson"].includes(selected) && (
        <DetailPanel {...details[selected]} onClose={() => setSelected(null)} />
      )}

      {/* Odds panel for Bernoulli */}
      {selected === "bernoulli" && (
        <div style={{ display: "flex", justifyContent: "center", marginTop: 8 }}>
          <Box
            label="π ↔ Odds ↔ Log-Odds ↔ Odds Ratio"
            sub="Click for conversion details"
            color={colors.bernoulli}
            bg={colors.bernoulliLight}
            onClick={() => toggle("odds")}
            width={300}
          />
        </div>
      )}
      {selected === "odds" && (
        <DetailPanel {...details.odds} onClose={() => setSelected(null)} />
      )}

      {/* Arrow to MLE */}
      <div style={{ display: "flex", justifyContent: "center" }}>
        <Arrow vertical label="estimated via" color={colors.mle} />
      </div>

      {/* MLE Machinery */}
      <div
        style={{
          background: colors.mleLight,
          border: `2px solid ${colors.mle}`,
          borderRadius: 12,
          padding: 16,
        }}
      >
        <div
          style={{
            display: "flex",
            justifyContent: "center",
            gap: 12,
            flexWrap: "wrap",
          }}
        >
          <Box
            label="MLE Estimation"
            sub="L(θ|y) → ℓ(θ|y) → Score → Hessian → Fisher Info"
            color={colors.mle}
            bg="white"
            onClick={() => toggle("mle")}
            width={320}
          />
          <Box
            label="Newton-Raphson"
            sub="β⁽ʲ⁺¹⁾ = β⁽ʲ⁾ − score × Hessian⁻¹"
            color={colors.mle}
            bg="white"
            onClick={() => toggle("nr")}
            width={280}
          />
        </div>
        <div
          style={{
            display: "flex",
            justifyContent: "center",
            gap: 16,
            marginTop: 10,
            flexWrap: "wrap",
          }}
        >
          <div
            style={{
              fontSize: 11,
              background: "white",
              borderRadius: 6,
              padding: "6px 12px",
              color: colors.gray,
              border: `1px solid #E5E7EB`,
            }}
          >
            Normal: closed-form β̂ = (X′X)⁻¹X′Y
          </div>
          <div
            style={{
              fontSize: 11,
              background: "white",
              borderRadius: 6,
              padding: "6px 12px",
              color: colors.gray,
              border: `1px solid #E5E7EB`,
            }}
          >
            Bernoulli & Poisson: no closed form → iterate N-R
          </div>
        </div>
        {selected && ["mle", "nr"].includes(selected) && (
          <DetailPanel {...details[selected]} onClose={() => setSelected(null)} />
        )}
      </div>

      {/* Legend */}
      <div
        style={{
          marginTop: 20,
          padding: 12,
          background: "white",
          borderRadius: 8,
          border: "1px solid #E5E7EB",
          display: "flex",
          justifyContent: "center",
          gap: 20,
          flexWrap: "wrap",
          fontSize: 11,
        }}
      >
        {[
          { c: colors.framework, l: "GLM Framework" },
          { c: colors.normal, l: "Normal/OLS" },
          { c: colors.bernoulli, l: "Bernoulli/Logit" },
          { c: colors.poisson, l: "Poisson/Count" },
          { c: colors.mle, l: "Estimation" },
        ].map(({ c, l }) => (
          <div
            key={l}
            style={{ display: "flex", alignItems: "center", gap: 4 }}
          >
            <div
              style={{
                width: 12,
                height: 12,
                background: c,
                borderRadius: 3,
              }}
            />
            <span style={{ color: "#374151" }}>{l}</span>
          </div>
        ))}
      </div>
    </div>
  );
}
