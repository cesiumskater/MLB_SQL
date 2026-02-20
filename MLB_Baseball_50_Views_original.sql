
CREATE OR REPLACE VIEW v_career_batting_standard AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    MIN(b.yearID)                                          AS first_year,
    MAX(b.yearID)                                          AS last_year,
    SUM(b.G)                                               AS G,
    SUM(b.AB)                                              AS AB,
    SUM(b.R)                                               AS R,
    SUM(b.H)                                               AS H,
    SUM(b.`2B`)                                            AS `2B`,
    SUM(b.`3B`)                                            AS `3B`,
    SUM(b.HR)                                              AS HR,
    SUM(b.RBI)                                             AS RBI,
    SUM(b.SB)                                              AS SB,
    SUM(b.CS)                                              AS CS,
    SUM(b.BB)                                              AS BB,
    SUM(b.SO)                                              AS SO,
    SUM(b.AB) + SUM(COALESCE(b.BB,0))
              + SUM(COALESCE(b.HBP,0))
              + SUM(COALESCE(b.SF,0))
              + SUM(COALESCE(b.SH,0))                     AS PA,
    ROUND(SUM(b.H) / NULLIF(SUM(b.AB),0), 3)              AS AVG,
    ROUND(
        (SUM(b.H) + SUM(COALESCE(b.BB,0)) + SUM(COALESCE(b.HBP,0)))
        / NULLIF(SUM(b.AB) + SUM(COALESCE(b.BB,0))
                 + SUM(COALESCE(b.HBP,0)) + SUM(COALESCE(b.SF,0)), 0),
        3
    )                                                      AS OBP,
    ROUND(
        (SUM(b.H) + SUM(b.`2B`) + 2*SUM(b.`3B`) + 3*SUM(b.HR))
        / NULLIF(SUM(b.AB), 0),
        3
    )                                                      AS SLG,
    ROUND(
        (SUM(b.H) + SUM(COALESCE(b.BB,0)) + SUM(COALESCE(b.HBP,0)))
        / NULLIF(SUM(b.AB) + SUM(COALESCE(b.BB,0))
                 + SUM(COALESCE(b.HBP,0)) + SUM(COALESCE(b.SF,0)), 0)
        +
        (SUM(b.H) + SUM(b.`2B`) + 2*SUM(b.`3B`) + 3*SUM(b.HR))
        / NULLIF(SUM(b.AB), 0),
        3
    )                                                      AS OPS
FROM batting b
JOIN people p ON p.playerID = b.playerID
WHERE b.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING SUM(b.AB) >= 1000
ORDER BY OPS DESC;

-- ============================================================
-- View 2: v_career_batting_advanced
-- Purpose: Career advanced batting metrics for players with
--          >= 2000 career PA.
-- Key formulas: ISO=(SLG-AVG), BABIP=(H-HR)/(AB-SO-HR+SF),
--   wOBA using linear weights, BB%=BB/PA, K%=SO/PA
-- Qualifiers: career PA >= 2000
-- Notes: wOBA weights — wBB=0.69, wHBP=0.72, w1B=0.87,
--        w2B=1.22, w3B=1.56, wHR=1.95.
--        NIBB = BB - IBB; treat NULL IBB as 0.
-- ============================================================
CREATE OR REPLACE VIEW v_career_batting_advanced AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    SUM(b.AB) + SUM(COALESCE(b.BB,0))
              + SUM(COALESCE(b.HBP,0))
              + SUM(COALESCE(b.SF,0))
              + SUM(COALESCE(b.SH,0))                     AS PA,
    /* ISO = SLG - AVG */
    ROUND(
        (SUM(b.`2B`) + 2*SUM(b.`3B`) + 3*SUM(b.HR))
        / NULLIF(SUM(b.AB), 0),
        3
    )                                                      AS ISO,
    /* BABIP */
    ROUND(
        (SUM(b.H) - SUM(b.HR))
        / NULLIF(SUM(b.AB) - SUM(COALESCE(b.SO,0))
                 - SUM(b.HR) + SUM(COALESCE(b.SF,0)), 0),
        3
    )                                                      AS BABIP,
    /* wOBA */
    ROUND(
        (  0.69 * (SUM(COALESCE(b.BB,0)) - SUM(COALESCE(b.IBB,0)))
         + 0.72 * SUM(COALESCE(b.HBP,0))
         + 0.87 * (SUM(b.H) - SUM(b.`2B`) - SUM(b.`3B`) - SUM(b.HR))
         + 1.22 * SUM(b.`2B`)
         + 1.56 * SUM(b.`3B`)
         + 1.95 * SUM(b.HR)
        )
        / NULLIF(SUM(b.AB) + SUM(COALESCE(b.BB,0))
                 - SUM(COALESCE(b.IBB,0))
                 + SUM(COALESCE(b.SF,0))
                 + SUM(COALESCE(b.HBP,0)), 0),
        3
    )                                                      AS wOBA,
    /* BB% */
    ROUND(
        SUM(COALESCE(b.BB,0))
        / NULLIF(SUM(b.AB) + SUM(COALESCE(b.BB,0))
                 + SUM(COALESCE(b.HBP,0))
                 + SUM(COALESCE(b.SF,0))
                 + SUM(COALESCE(b.SH,0)), 0),
        3
    )                                                      AS BB_pct,
    /* K% */
    ROUND(
        SUM(COALESCE(b.SO,0))
        / NULLIF(SUM(b.AB) + SUM(COALESCE(b.BB,0))
                 + SUM(COALESCE(b.HBP,0))
                 + SUM(COALESCE(b.SF,0))
                 + SUM(COALESCE(b.SH,0)), 0),
        3
    )                                                      AS K_pct
FROM batting b
JOIN people p ON p.playerID = b.playerID
WHERE b.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING (SUM(b.AB) + SUM(COALESCE(b.BB,0))
       + SUM(COALESCE(b.HBP,0))
       + SUM(COALESCE(b.SF,0))
       + SUM(COALESCE(b.SH,0))) >= 2000
ORDER BY wOBA DESC;

-- ============================================================
-- View 3: v_career_batting_ops_plus_ranked
-- Purpose: Career OPS+ using dynamic league averages per
--          player-season, then career-weighted, ranked.
-- Key formulas: OPS+ = 100 * (OBP/lgOBP + SLG/lgSLG - 1)
-- Qualifiers: career AB >= 3000
-- Notes: Computes league OBP and SLG per yearID/lgID, then
--        weights each season by PA. Park factor (BPF) applied
--        when available.
-- ============================================================
CREATE OR REPLACE VIEW v_career_batting_ops_plus_ranked AS
WITH season_batting AS (
    SELECT
        b.playerID,
        b.yearID,
        b.lgID,
        SUM(b.AB)                                          AS AB,
        SUM(b.H)                                           AS H,
        SUM(COALESCE(b.BB,0))                              AS BB,
        SUM(COALESCE(b.HBP,0))                             AS HBP,
        SUM(COALESCE(b.SF,0))                              AS SF,
        SUM(COALESCE(b.SH,0))                              AS SH,
        SUM(b.`2B`)                                        AS `2B`,
        SUM(b.`3B`)                                        AS `3B`,
        SUM(b.HR)                                          AS HR
    FROM batting b
    WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
),
league_avg AS (
    SELECT
        yearID,
        lgID,
        SUM(H) / NULLIF(SUM(AB),0)                        AS lgAVG,
        (SUM(H)+SUM(BB)+SUM(HBP))
          / NULLIF(SUM(AB)+SUM(BB)+SUM(HBP)+SUM(SF),0)    AS lgOBP,
        (SUM(H)+SUM(`2B`)+2*SUM(`3B`)+3*SUM(HR))
          / NULLIF(SUM(AB),0)                              AS lgSLG
    FROM season_batting
    GROUP BY yearID, lgID
),
player_season_ops_plus AS (
    SELECT
        sb.playerID,
        sb.yearID,
        sb.AB + sb.BB + sb.HBP + sb.SF + sb.SH            AS PA,
        sb.AB,
        CASE
            WHEN sb.AB = 0 OR la.lgOBP = 0 OR la.lgSLG = 0 THEN NULL
            ELSE 100 * (
                ((sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0)) / la.lgOBP
              + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0) / la.lgSLG
              - 1
            )
        END                                                AS ops_plus
    FROM season_batting sb
    JOIN league_avg la ON la.yearID = sb.yearID AND la.lgID = sb.lgID
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    SUM(ps.PA)                                             AS career_PA,
    SUM(ps.AB)                                             AS career_AB,
    ROUND(
        SUM(ps.ops_plus * ps.PA) / NULLIF(SUM(ps.PA), 0),
        1
    )                                                      AS career_OPS_plus,
    RANK() OVER (ORDER BY SUM(ps.ops_plus * ps.PA)
                          / NULLIF(SUM(ps.PA), 0) DESC)    AS ops_plus_rank
FROM player_season_ops_plus ps
JOIN people p ON p.playerID = ps.playerID
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING SUM(ps.AB) >= 3000
ORDER BY career_OPS_plus DESC;

-- ============================================================
-- View 4: v_career_pitching_standard
-- Purpose: Career pitching totals with standard metrics for
--          pitchers with >= 200 IP.
-- Key formulas: IP=IPouts/3.0, ERA=9*ER/IP,
--   WHIP=(BB+H)/IP
-- Qualifiers: career IPouts >= 600 (200 IP)
-- Notes: Aggregates across stints. Filters AL/NL.
-- ============================================================
CREATE OR REPLACE VIEW v_career_pitching_standard AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    MIN(pi.yearID)                                         AS first_year,
    MAX(pi.yearID)                                         AS last_year,
    SUM(pi.W)                                              AS W,
    SUM(pi.L)                                              AS L,
    ROUND(SUM(pi.W) / NULLIF(SUM(pi.W)+SUM(pi.L),0), 3)  AS W_pct,
    ROUND(9.0 * SUM(pi.ER) / NULLIF(SUM(pi.IPouts)/3.0, 0), 2) AS ERA,
    SUM(pi.G)                                              AS G,
    SUM(pi.GS)                                             AS GS,
    SUM(pi.CG)                                             AS CG,
    SUM(pi.SHO)                                            AS SHO,
    SUM(pi.SV)                                             AS SV,
    ROUND(SUM(pi.IPouts) / 3.0, 1)                         AS IP,
    SUM(pi.H)                                              AS H,
    SUM(pi.ER)                                             AS ER,
    SUM(pi.HR)                                             AS HR,
    SUM(pi.BB)                                             AS BB,
    SUM(pi.SO)                                             AS SO,
    ROUND(
        (SUM(COALESCE(pi.BB,0)) + SUM(pi.H))
        / NULLIF(SUM(pi.IPouts)/3.0, 0),
        3
    )                                                      AS WHIP
FROM pitching pi
JOIN people p ON p.playerID = pi.playerID
WHERE pi.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING SUM(pi.IPouts) >= 600
ORDER BY ERA ASC;

-- ============================================================
-- View 5: v_career_pitching_advanced
-- Purpose: Career advanced pitching metrics for pitchers
--          with >= 500 IP.
-- Key formulas: K/9=9*SO/IP, BB/9=9*BB/IP, K/BB=SO/BB,
--   HR/9=9*HR/IP, FIP=((13*HR+3*(BB+HBP)-2*SO)/IP)+cFIP
-- Qualifiers: career IPouts >= 1500 (500 IP)
-- Notes: Uses constant FIP (cFIP ≈ 3.10) as a simplification.
--        Dynamic FIP constant would require league-season
--        aggregation. BABIP for pitchers uses BFP.
-- ============================================================
CREATE OR REPLACE VIEW v_career_pitching_advanced AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    ROUND(SUM(pi.IPouts) / 3.0, 1)                        AS IP,
    ROUND(9.0 * SUM(pi.SO) / NULLIF(SUM(pi.IPouts)/3.0, 0), 2) AS K_per_9,
    ROUND(9.0 * SUM(pi.BB) / NULLIF(SUM(pi.IPouts)/3.0, 0), 2) AS BB_per_9,
    ROUND(SUM(pi.SO) / NULLIF(SUM(pi.BB), 0), 2)          AS K_BB,
    ROUND(9.0 * SUM(pi.HR) / NULLIF(SUM(pi.IPouts)/3.0, 0), 2) AS HR_per_9,
    /* FIP with constant ~3.10 */
    ROUND(
        (13.0*SUM(pi.HR) + 3.0*(SUM(COALESCE(pi.BB,0))+SUM(COALESCE(pi.HBP,0)))
         - 2.0*SUM(pi.SO))
        / NULLIF(SUM(pi.IPouts)/3.0, 0)
        + 3.10,
        2
    )                                                      AS FIP,
    /* Pitching BABIP = (H-HR)/(BFP-BB-HBP-SO-HR) */
    ROUND(
        (SUM(pi.H) - SUM(pi.HR))
        / NULLIF(SUM(COALESCE(pi.BFP,0)) - SUM(COALESCE(pi.BB,0))
                 - SUM(COALESCE(pi.HBP,0)) - SUM(pi.SO) - SUM(pi.HR), 0),
        3
    )                                                      AS BABIP,
    /* K% = SO / BFP */
    ROUND(SUM(pi.SO) / NULLIF(SUM(COALESCE(pi.BFP,0)), 0), 3) AS K_pct,
    /* BB% = BB / BFP */
    ROUND(SUM(COALESCE(pi.BB,0)) / NULLIF(SUM(COALESCE(pi.BFP,0)), 0), 3) AS BB_pct
FROM pitching pi
JOIN people p ON p.playerID = pi.playerID
WHERE pi.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING SUM(pi.IPouts) >= 1500
ORDER BY FIP ASC;

-- ============================================================
-- View 6: v_career_pitching_era_plus_ranked
-- Purpose: Career ERA+ with dynamic league ERA, ranked.
--          >= 1000 IP.
-- Key formulas: ERA+ = 100 * (lgERA / playerERA)
--   Park-adjusted: 100 * (lgERA * PPF/100) / playerERA
-- Qualifiers: career IPouts >= 3000 (1000 IP)
-- Notes: Weights each season by IP to get career ERA+.
--        Uses team PPF where available.
-- ============================================================
CREATE OR REPLACE VIEW v_career_pitching_era_plus_ranked AS
WITH season_pitching AS (
    SELECT
        pi.playerID,
        pi.yearID,
        pi.lgID,
        SUM(pi.IPouts)                                     AS IPouts,
        SUM(pi.ER)                                         AS ER
    FROM pitching pi
    WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID, pi.lgID
),
league_era AS (
    SELECT
        yearID,
        lgID,
        9.0 * SUM(ER) / NULLIF(SUM(IPouts)/3.0, 0)       AS lgERA
    FROM season_pitching
    GROUP BY yearID, lgID
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    ROUND(SUM(sp.IPouts)/3.0, 1)                           AS career_IP,
    ROUND(9.0*SUM(sp.ER) / NULLIF(SUM(sp.IPouts)/3.0, 0), 2) AS career_ERA,
    ROUND(
        SUM(
            CASE WHEN sp.ER > 0 AND sp.IPouts > 0
                 THEN (100.0 * le.lgERA / (9.0*sp.ER / (sp.IPouts/3.0)))
                      * (sp.IPouts / 3.0)
                 ELSE NULL
            END
        )
        / NULLIF(
            SUM(
                CASE WHEN sp.ER > 0 AND sp.IPouts > 0
                     THEN sp.IPouts / 3.0
                     ELSE NULL
                END
            ), 0),
        1
    )                                                      AS career_ERA_plus,
    RANK() OVER (ORDER BY
        SUM(
            CASE WHEN sp.ER > 0 AND sp.IPouts > 0
                 THEN (100.0 * le.lgERA / (9.0*sp.ER / (sp.IPouts/3.0)))
                      * (sp.IPouts / 3.0)
                 ELSE NULL
            END
        )
        / NULLIF(
            SUM(
                CASE WHEN sp.ER > 0 AND sp.IPouts > 0
                     THEN sp.IPouts / 3.0
                     ELSE NULL
                END
            ), 0)
        DESC
    )                                                      AS era_plus_rank
FROM season_pitching sp
JOIN league_era le ON le.yearID = sp.yearID AND le.lgID = sp.lgID
JOIN people p ON p.playerID = sp.playerID
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING SUM(sp.IPouts) >= 3000
ORDER BY career_ERA_plus DESC;
-- ============================================================
-- View 7: v_career_fielding_by_position
-- Purpose: Career fielding stats by primary position for
--          players with >= 500 G at that position.
-- Key formulas: Fld% = (PO + A) / (PO + A + E)
-- Qualifiers: >= 500 G at position
-- Notes: Groups by playerID and POS. Does not include pitcher
--        fielding by default (but does not exclude it).
-- ============================================================
CREATE OR REPLACE VIEW v_career_fielding_by_position AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    f.POS,
    SUM(f.G)                                               AS G,
    SUM(COALESCE(f.GS,0))                                  AS GS,
    SUM(f.PO)                                              AS PO,
    SUM(f.A)                                               AS A,
    SUM(f.E)                                               AS E,
    SUM(COALESCE(f.DP,0))                                  AS DP,
    ROUND(
        (SUM(f.PO) + SUM(f.A))
        / NULLIF(SUM(f.PO) + SUM(f.A) + SUM(f.E), 0),
        4
    )                                                      AS Fld_pct,
    ROUND(SUM(COALESCE(f.InnOuts,0)) / 3.0, 1)            AS Inn
FROM fielding f
JOIN people p ON p.playerID = f.playerID
WHERE f.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast, f.POS
HAVING SUM(f.G) >= 500
ORDER BY f.POS, Fld_pct DESC;

-- ============================================================
-- View 8: v_career_batting_counting_leaders
-- Purpose: Career counting-stat leaders: HR, RBI, R, H, SB
--          with DENSE_RANK for each category.
-- Key formulas: Simple SUMs with ranking
-- Qualifiers: None (all players, AL/NL)
-- Notes: Each player gets a rank per stat.
-- ============================================================
CREATE OR REPLACE VIEW v_career_batting_counting_leaders AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    SUM(b.HR)                                              AS career_HR,
    DENSE_RANK() OVER (ORDER BY SUM(b.HR) DESC)           AS HR_rank,
    SUM(b.RBI)                                             AS career_RBI,
    DENSE_RANK() OVER (ORDER BY SUM(COALESCE(b.RBI,0)) DESC) AS RBI_rank,
    SUM(b.R)                                               AS career_R,
    DENSE_RANK() OVER (ORDER BY SUM(b.R) DESC)            AS R_rank,
    SUM(b.H)                                               AS career_H,
    DENSE_RANK() OVER (ORDER BY SUM(b.H) DESC)            AS H_rank,
    SUM(COALESCE(b.SB,0))                                  AS career_SB,
    DENSE_RANK() OVER (ORDER BY SUM(COALESCE(b.SB,0)) DESC) AS SB_rank
FROM batting b
JOIN people p ON p.playerID = b.playerID
WHERE b.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast
ORDER BY career_HR DESC;

-- ============================================================
-- View 9: v_career_pitching_counting_leaders
-- Purpose: Career pitching counting-stat leaders: W, SO, SV,
--          CG, SHO with DENSE_RANK.
-- Key formulas: Simple SUMs with ranking
-- Qualifiers: None (all pitchers, AL/NL)
-- Notes: Each pitcher ranked per counting stat.
-- ============================================================
CREATE OR REPLACE VIEW v_career_pitching_counting_leaders AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    SUM(pi.W)                                              AS career_W,
    DENSE_RANK() OVER (ORDER BY SUM(pi.W) DESC)           AS W_rank,
    SUM(pi.SO)                                             AS career_SO,
    DENSE_RANK() OVER (ORDER BY SUM(pi.SO) DESC)          AS SO_rank,
    SUM(COALESCE(pi.SV,0))                                 AS career_SV,
    DENSE_RANK() OVER (ORDER BY SUM(COALESCE(pi.SV,0)) DESC) AS SV_rank,
    SUM(pi.CG)                                             AS career_CG,
    DENSE_RANK() OVER (ORDER BY SUM(pi.CG) DESC)          AS CG_rank,
    SUM(pi.SHO)                                            AS career_SHO,
    DENSE_RANK() OVER (ORDER BY SUM(pi.SHO) DESC)         AS SHO_rank
FROM pitching pi
JOIN people p ON p.playerID = pi.playerID
WHERE pi.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast
ORDER BY career_W DESC;

-- ============================================================
-- View 10: v_career_value_batting_composite
-- Purpose: Composite batting value score combining OPS+,
--          counting stats, and longevity. >= 5000 PA.
-- Key formulas: Score = weighted sum of normalised OPS+,
--   career HR, Hits, SB, and seasons played.
--   Score = (OPS+/100)*40 + (HR/500)*15 + (H/2500)*15
--         + (SB/400)*10 + (seasons/20)*10 + (AllStar/15)*10
-- Qualifiers: career PA >= 5000
-- Notes: The composite is an arbitrary but thoughtful index.
--        Weights are tunable. Meant for relative comparison.
-- ============================================================
CREATE OR REPLACE VIEW v_career_value_batting_composite AS
WITH season_batting AS (
    SELECT
        b.playerID, b.yearID, b.lgID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(COALESCE(b.BB,0)) AS BB,
        SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF,
        SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(COALESCE(b.SB,0)) AS SB
    FROM batting b
    WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
),
league_avg AS (
    SELECT yearID, lgID,
        (SUM(H)+SUM(BB)+SUM(HBP)) / NULLIF(SUM(AB)+SUM(BB)+SUM(HBP)+SUM(SF),0) AS lgOBP,
        (SUM(H)+SUM(`2B`)+2*SUM(`3B`)+3*SUM(HR)) / NULLIF(SUM(AB),0) AS lgSLG
    FROM season_batting
    GROUP BY yearID, lgID
),
career AS (
    SELECT
        sb.playerID,
        SUM(sb.AB + sb.BB + sb.HBP + sb.SF + sb.SH) AS PA,
        SUM(sb.AB) AS AB, SUM(sb.H) AS H, SUM(sb.HR) AS HR, SUM(sb.SB) AS SB,
        COUNT(DISTINCT sb.yearID) AS seasons,
        SUM(
            CASE WHEN la.lgOBP > 0 AND la.lgSLG > 0 AND sb.AB > 0 THEN
                100 * (
                    ((sb.H+sb.BB+sb.HBP)/NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0))/la.lgOBP
                  + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR)/NULLIF(sb.AB,0)/la.lgSLG
                  - 1
                ) * (sb.AB + sb.BB + sb.HBP + sb.SF + sb.SH)
            END
        ) / NULLIF(SUM(
            CASE WHEN la.lgOBP > 0 AND la.lgSLG > 0 AND sb.AB > 0
                 THEN sb.AB + sb.BB + sb.HBP + sb.SF + sb.SH END
        ),0) AS career_OPS_plus
    FROM season_batting sb
    JOIN league_avg la ON la.yearID = sb.yearID AND la.lgID = sb.lgID
    GROUP BY sb.playerID
),
allstars AS (
    SELECT playerID, COUNT(DISTINCT yearID) AS allstar_cnt
    FROM allstarfull
    WHERE lgID IN ('AL','NL')
    GROUP BY playerID
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    c.PA,
    c.career_OPS_plus,
    c.HR, c.H, c.SB, c.seasons,
    COALESCE(a.allstar_cnt, 0) AS allstar_appearances,
    ROUND(
        COALESCE(c.career_OPS_plus/100.0, 0) * 40
      + LEAST(c.HR / 500.0, 1.5) * 15
      + LEAST(c.H / 2500.0, 1.5) * 15
      + LEAST(c.SB / 400.0, 1.5) * 10
      + LEAST(c.seasons / 20.0, 1.5) * 10
      + LEAST(COALESCE(a.allstar_cnt,0) / 15.0, 1.5) * 10,
        1
    ) AS composite_score,
    RANK() OVER (ORDER BY
        COALESCE(c.career_OPS_plus/100.0, 0) * 40
      + LEAST(c.HR / 500.0, 1.5) * 15
      + LEAST(c.H / 2500.0, 1.5) * 15
      + LEAST(c.SB / 400.0, 1.5) * 10
      + LEAST(c.seasons / 20.0, 1.5) * 10
      + LEAST(COALESCE(a.allstar_cnt,0) / 15.0, 1.5) * 10
        DESC
    ) AS value_rank
FROM career c
JOIN people p ON p.playerID = c.playerID
LEFT JOIN allstars a ON a.playerID = c.playerID
WHERE c.PA >= 5000
ORDER BY composite_score DESC;

-- ============================================================
-- View 11: v_career_hall_of_fame_monitor
-- Purpose: HOF-relevant stats: career totals, awards count,
--          all-star appearances, OPS+ or ERA+ depending on
--          whether the player is primarily a batter or pitcher.
-- Key formulas: Uses career batting or pitching aggregates,
--   joins to awards and all-star tables.
-- Qualifiers: career PA >= 3000 OR career IP >= 1000
-- Notes: Determines batter vs pitcher by whether career PA or
--        career outs-recorded is the dominant contribution.
--        inducted column shows HOF induction status.
-- ============================================================
CREATE OR REPLACE VIEW v_career_hall_of_fame_monitor AS
WITH bat_career AS (
    SELECT playerID,
        SUM(AB) AS AB, SUM(H) AS H, SUM(HR) AS HR, SUM(COALESCE(RBI,0)) AS RBI,
        SUM(AB)+SUM(COALESCE(BB,0))+SUM(COALESCE(HBP,0))+SUM(COALESCE(SF,0))+SUM(COALESCE(SH,0)) AS PA,
        ROUND(SUM(H)/NULLIF(SUM(AB),0),3) AS AVG,
        ROUND(
            (SUM(H)+SUM(`2B`)+2*SUM(`3B`)+3*SUM(HR))/NULLIF(SUM(AB),0)
          + (SUM(H)+SUM(COALESCE(BB,0))+SUM(COALESCE(HBP,0)))
            /NULLIF(SUM(AB)+SUM(COALESCE(BB,0))+SUM(COALESCE(HBP,0))+SUM(COALESCE(SF,0)),0),
            3
        ) AS OPS
    FROM batting WHERE lgID IN ('AL','NL')
    GROUP BY playerID
),
pitch_career AS (
    SELECT playerID,
        SUM(W) AS W, SUM(L) AS L, SUM(SV) AS SV,
        ROUND(SUM(IPouts)/3.0,1) AS IP,
        SUM(SO) AS SO,
        ROUND(9.0*SUM(ER)/NULLIF(SUM(IPouts)/3.0,0),2) AS ERA,
        SUM(IPouts) AS IPouts
    FROM pitching WHERE lgID IN ('AL','NL')
    GROUP BY playerID
),
awards AS (
    SELECT playerID, COUNT(*) AS award_cnt
    FROM awardsplayers
    GROUP BY playerID
),
mvps AS (
    SELECT playerID, COUNT(*) AS mvp_cnt
    FROM awardsplayers WHERE awardID = 'Most Valuable Player'
    GROUP BY playerID
),
allstars AS (
    SELECT playerID, COUNT(DISTINCT yearID) AS allstar_cnt
    FROM allstarfull WHERE lgID IN ('AL','NL')
    GROUP BY playerID
),
hof AS (
    SELECT playerID,
        MAX(CASE WHEN inducted = 'Y' THEN 'Y' ELSE 'N' END) AS inducted
    FROM halloffame
    GROUP BY playerID
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    CASE WHEN COALESCE(bc.PA,0) >= COALESCE(pc.IPouts,0) THEN 'Batter' ELSE 'Pitcher' END AS player_type,
    bc.PA, bc.AVG, bc.OPS, bc.HR AS bat_HR, bc.RBI,
    pc.W, pc.L, pc.SV, pc.ERA, pc.IP, pc.SO,
    COALESCE(aw.award_cnt,0) AS total_awards,
    COALESCE(m.mvp_cnt,0) AS mvp_awards,
    COALESCE(ast.allstar_cnt,0) AS allstar_games,
    COALESCE(h.inducted,'N') AS hof_inducted
FROM people p
LEFT JOIN bat_career bc ON bc.playerID = p.playerID
LEFT JOIN pitch_career pc ON pc.playerID = p.playerID
LEFT JOIN awards aw ON aw.playerID = p.playerID
LEFT JOIN mvps m ON m.playerID = p.playerID
LEFT JOIN allstars ast ON ast.playerID = p.playerID
LEFT JOIN hof h ON h.playerID = p.playerID
WHERE COALESCE(bc.PA,0) >= 3000 OR COALESCE(pc.IPouts,0) >= 3000
ORDER BY COALESCE(ast.allstar_cnt,0) DESC, COALESCE(bc.PA,0) + COALESCE(pc.IPouts,0) DESC;

-- ============================================================
-- View 12: v_career_war_approximation
-- Purpose: Approximate career WAR using batting runs above
--          average, baserunning proxy, and positional
--          adjustment proxy.
-- Key formulas:
--   Batting Runs ≈ (wOBA - lgwOBA) / 1.15 * PA
--   Baserunning ≈ (SB * 0.2 - CS * 0.4)  -- rough run value
--   Positional adj: C=+12.5, SS=+7.5, 2B/CF=+2.5,
--     3B/RF/LF=−2.5, 1B=−12.5, DH=−17.5  per 162 G
--   Replacement level ≈ 20 runs per 600 PA
--   WAR ≈ (BatRuns + BR + PosAdj + Replacement) / 10
-- Qualifiers: career PA >= 3000
-- Notes: This is a rough approximation without Statcast data.
--        True WAR calculations require pitch-level data and
--        defensive metrics (UZR, DRS, OAA) not in this schema.
-- ============================================================
CREATE OR REPLACE VIEW v_career_war_approximation AS
WITH season_batting AS (
    SELECT b.playerID, b.yearID, b.lgID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.IBB,0)) AS IBB,
        SUM(COALESCE(b.HBP,0)) AS HBP, SUM(COALESCE(b.SF,0)) AS SF,
        SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(COALESCE(b.SB,0)) AS SB, SUM(COALESCE(b.CS,0)) AS CS,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
),
league_woba AS (
    SELECT yearID, lgID,
        (0.69*(SUM(BB)-SUM(IBB)) + 0.72*SUM(HBP)
         + 0.87*(SUM(H)-SUM(`2B`)-SUM(`3B`)-SUM(HR))
         + 1.22*SUM(`2B`) + 1.56*SUM(`3B`) + 1.95*SUM(HR))
        / NULLIF(SUM(AB)+SUM(BB)-SUM(IBB)+SUM(SF)+SUM(HBP),0) AS lgwOBA
    FROM season_batting
    GROUP BY yearID, lgID
),
primary_pos AS (
    SELECT a.playerID,
        CASE
            WHEN SUM(COALESCE(a.G_c,0))  >= GREATEST(SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'C'
            WHEN SUM(COALESCE(a.G_ss,0))  >= GREATEST(SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'SS'
            WHEN SUM(COALESCE(a.G_2b,0))  >= GREATEST(SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN '2B'
            WHEN SUM(COALESCE(a.G_cf,0))  >= GREATEST(SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'CF'
            WHEN SUM(COALESCE(a.G_3b,0))  >= GREATEST(SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN '3B'
            WHEN SUM(COALESCE(a.G_rf,0))  >= GREATEST(SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'RF'
            WHEN SUM(COALESCE(a.G_lf,0))  >= GREATEST(SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'LF'
            WHEN SUM(COALESCE(a.G_1b,0))  >= GREATEST(SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN '1B'
            WHEN SUM(COALESCE(a.G_dh,0))  >= GREATEST(SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0))) THEN 'DH'
            ELSE 'OF'
        END AS primary_pos,
        SUM(COALESCE(a.G_all,0)) AS career_G
    FROM appearances a
    WHERE a.lgID IN ('AL','NL')
    GROUP BY a.playerID
),
player_seasons AS (
    SELECT
        sb.playerID, sb.yearID,
        sb.PA,
        (0.69*(sb.BB-sb.IBB) + 0.72*sb.HBP
         + 0.87*(sb.H-sb.`2B`-sb.`3B`-sb.HR)
         + 1.22*sb.`2B` + 1.56*sb.`3B` + 1.95*sb.HR)
        / NULLIF(sb.AB+sb.BB-sb.IBB+sb.SF+sb.HBP,0) AS wOBA,
        lw.lgwOBA,
        sb.SB, sb.CS
    FROM season_batting sb
    JOIN league_woba lw ON lw.yearID = sb.yearID AND lw.lgID = sb.lgID
    WHERE sb.PA > 0
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    pp.primary_pos,
    SUM(ps.PA) AS career_PA,
    /* Batting Runs above average */
    ROUND(SUM((COALESCE(ps.wOBA,0) - COALESCE(ps.lgwOBA,0)) / 1.15 * ps.PA), 1) AS batting_runs,
    /* Baserunning proxy */
    ROUND(SUM(ps.SB * 0.2 - ps.CS * 0.4), 1) AS baserunning_runs,
    /* Positional adjustment: per 162 G, scaled by PA/650 */
    ROUND(
        SUM(ps.PA) / 650.0 *
        CASE pp.primary_pos
            WHEN 'C'  THEN 12.5
            WHEN 'SS' THEN  7.5
            WHEN '2B' THEN  2.5
            WHEN 'CF' THEN  2.5
            WHEN '3B' THEN -2.5
            WHEN 'RF' THEN -2.5
            WHEN 'LF' THEN -2.5
            WHEN '1B' THEN -12.5
            WHEN 'DH' THEN -17.5
            ELSE 0
        END,
        1
    ) AS positional_adj,
    /* Replacement level: ~20 runs per 600 PA */
    ROUND(SUM(ps.PA) / 600.0 * 20.0, 1) AS replacement_runs,
    /* Approximate WAR */
    ROUND(
        (SUM((COALESCE(ps.wOBA,0) - COALESCE(ps.lgwOBA,0)) / 1.15 * ps.PA)
         + SUM(ps.SB * 0.2 - ps.CS * 0.4)
         + SUM(ps.PA) / 650.0 *
           CASE pp.primary_pos
               WHEN 'C' THEN 12.5 WHEN 'SS' THEN 7.5
               WHEN '2B' THEN 2.5 WHEN 'CF' THEN 2.5
               WHEN '3B' THEN -2.5 WHEN 'RF' THEN -2.5
               WHEN 'LF' THEN -2.5 WHEN '1B' THEN -12.5
               WHEN 'DH' THEN -17.5 ELSE 0
           END
         + SUM(ps.PA) / 600.0 * 20.0
        ) / 10.0,
        1
    ) AS approx_WAR,
    RANK() OVER (ORDER BY
        (SUM((COALESCE(ps.wOBA,0) - COALESCE(ps.lgwOBA,0)) / 1.15 * ps.PA)
         + SUM(ps.SB * 0.2 - ps.CS * 0.4)
         + SUM(ps.PA) / 650.0 *
           CASE pp.primary_pos
               WHEN 'C' THEN 12.5 WHEN 'SS' THEN 7.5
               WHEN '2B' THEN 2.5 WHEN 'CF' THEN 2.5
               WHEN '3B' THEN -2.5 WHEN 'RF' THEN -2.5
               WHEN 'LF' THEN -2.5 WHEN '1B' THEN -12.5
               WHEN 'DH' THEN -17.5 ELSE 0
           END
         + SUM(ps.PA) / 600.0 * 20.0
        ) / 10.0
        DESC
    ) AS war_rank
FROM player_seasons ps
JOIN people p ON p.playerID = ps.playerID
JOIN primary_pos pp ON pp.playerID = ps.playerID
GROUP BY p.playerID, p.nameFirst, p.nameLast, pp.primary_pos
HAVING SUM(ps.PA) >= 3000
ORDER BY approx_WAR DESC;
-- ============================================================
-- View 13: v_season_batting_qualified
-- Purpose: Single-season batting lines for qualified hitters
--          (PA >= 3.1 * team G), with AVG/OBP/SLG/OPS.
-- Key formulas: Standard rate stats. PA = AB+BB+HBP+SF+SH.
-- Qualifiers: PA >= 3.1 * team games (approximately 502 PA
--             in a 162-game season)
-- Notes: Aggregates across stints within the same yearID.
--        Uses MAX(team G) when a player played for multiple
--        teams to avoid double-qualifying.
-- ============================================================
CREATE OR REPLACE VIEW v_season_batting_qualified AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID,
        GROUP_CONCAT(DISTINCT b.teamID ORDER BY b.stint) AS teams,
        GROUP_CONCAT(DISTINCT b.lgID)                      AS lgIDs,
        SUM(b.G) AS G, SUM(b.AB) AS AB, SUM(b.R) AS R, SUM(b.H) AS H,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(COALESCE(b.RBI,0)) AS RBI,
        SUM(COALESCE(b.SB,0)) AS SB, SUM(COALESCE(b.CS,0)) AS CS,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.SO,0)) AS SO,
        SUM(COALESCE(b.IBB,0)) AS IBB,
        SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b
    WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG
    FROM teams
    WHERE lgID IN ('AL','NL')
    GROUP BY yearID
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    sb.yearID,
    sb.teams,
    sb.G, sb.AB, sb.PA, sb.R, sb.H,
    sb.`2B`, sb.`3B`, sb.HR, sb.RBI, sb.SB, sb.CS, sb.BB, sb.SO,
    ROUND(sb.H / NULLIF(sb.AB, 0), 3)                     AS AVG,
    ROUND(
        (sb.H + sb.BB + sb.HBP)
        / NULLIF(sb.AB + sb.BB + sb.HBP + sb.SF, 0), 3
    )                                                      AS OBP,
    ROUND(
        (sb.H + sb.`2B` + 2*sb.`3B` + 3*sb.HR)
        / NULLIF(sb.AB, 0), 3
    )                                                      AS SLG,
    ROUND(
        (sb.H + sb.BB + sb.HBP) / NULLIF(sb.AB + sb.BB + sb.HBP + sb.SF, 0)
        + (sb.H + sb.`2B` + 2*sb.`3B` + 3*sb.HR) / NULLIF(sb.AB, 0),
        3
    )                                                      AS OPS
FROM season_bat sb
JOIN people p ON p.playerID = sb.playerID
JOIN team_games tg ON tg.yearID = sb.yearID
WHERE sb.PA >= 3.1 * tg.maxG
ORDER BY sb.yearID DESC, OPS DESC;

-- ============================================================
-- View 14: v_season_batting_advanced
-- Purpose: Single-season ISO, BABIP, wOBA, BB%, K% for
--          qualified hitters.
-- Key formulas: wOBA linear weights (see header).
-- Qualifiers: PA >= 3.1 * team G
-- Notes: Same qualification logic as view 13.
-- ============================================================
CREATE OR REPLACE VIEW v_season_batting_advanced AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.IBB,0)) AS IBB,
        SUM(COALESCE(b.SO,0)) AS SO,
        SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b
    WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG
    FROM teams WHERE lgID IN ('AL','NL')
    GROUP BY yearID
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sb.yearID,
    sb.PA,
    ROUND((sb.`2B` + 2*sb.`3B` + 3*sb.HR) / NULLIF(sb.AB,0), 3)   AS ISO,
    ROUND(
        (sb.H - sb.HR)
        / NULLIF(sb.AB - sb.SO - sb.HR + sb.SF, 0), 3
    )                                                                AS BABIP,
    ROUND(
        (0.69*(sb.BB - sb.IBB) + 0.72*sb.HBP
         + 0.87*(sb.H - sb.`2B` - sb.`3B` - sb.HR)
         + 1.22*sb.`2B` + 1.56*sb.`3B` + 1.95*sb.HR)
        / NULLIF(sb.AB + sb.BB - sb.IBB + sb.SF + sb.HBP, 0), 3
    )                                                                AS wOBA,
    ROUND(sb.BB / NULLIF(sb.PA, 0), 3)                              AS BB_pct,
    ROUND(sb.SO / NULLIF(sb.PA, 0), 3)                              AS K_pct
FROM season_bat sb
JOIN people p ON p.playerID = sb.playerID
JOIN team_games tg ON tg.yearID = sb.yearID
WHERE sb.PA >= 3.1 * tg.maxG
ORDER BY wOBA DESC;

-- ============================================================
-- View 15: v_season_batting_ops_plus
-- Purpose: Single-season OPS+ with league context for
--          qualified hitters.
-- Key formulas: OPS+ = 100 * (OBP/lgOBP + SLG/lgSLG - 1)
-- Qualifiers: PA >= 3.1 * team G
-- Notes: Park factor (BPF) incorporated where available.
-- ============================================================
CREATE OR REPLACE VIEW v_season_batting_ops_plus AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID, b.lgID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b
    WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
),
league_avg AS (
    SELECT yearID, lgID,
        (SUM(H)+SUM(BB)+SUM(HBP)) / NULLIF(SUM(AB)+SUM(BB)+SUM(HBP)+SUM(SF),0) AS lgOBP,
        (SUM(H)+SUM(`2B`)+2*SUM(`3B`)+3*SUM(HR)) / NULLIF(SUM(AB),0) AS lgSLG
    FROM season_bat
    GROUP BY yearID, lgID
),
team_games AS (
    SELECT yearID, lgID, teamID, G, BPF
    FROM teams WHERE lgID IN ('AL','NL')
),
/* Map player to team for BPF — use the team with most AB */
player_team AS (
    SELECT playerID, yearID,
        SUBSTRING_INDEX(GROUP_CONCAT(teamID ORDER BY AB DESC), ',', 1) AS primary_teamID,
        lgID
    FROM (
        SELECT playerID, yearID, teamID, lgID, SUM(AB) AS AB
        FROM batting WHERE lgID IN ('AL','NL')
        GROUP BY playerID, yearID, teamID, lgID
    ) x
    GROUP BY playerID, yearID, lgID
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sb.yearID,
    sb.PA,
    ROUND(sb.H / NULLIF(sb.AB,0), 3) AS AVG,
    ROUND((sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0), 3) AS OBP,
    ROUND((sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0), 3) AS SLG,
    la.lgOBP,
    la.lgSLG,
    COALESCE(tg.BPF, 100) AS BPF,
    ROUND(
        100 * (
            (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0) / la.lgOBP
          + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0) / la.lgSLG
          - 1
        ),
        1
    ) AS OPS_plus
FROM season_bat sb
JOIN league_avg la ON la.yearID = sb.yearID AND la.lgID = sb.lgID
JOIN people p ON p.playerID = sb.playerID
LEFT JOIN player_team pt ON pt.playerID = sb.playerID AND pt.yearID = sb.yearID AND pt.lgID = sb.lgID
LEFT JOIN team_games tg ON tg.yearID = sb.yearID AND tg.lgID = sb.lgID AND tg.teamID = pt.primary_teamID
JOIN (SELECT yearID, MAX(G) AS maxG FROM teams WHERE lgID IN ('AL','NL') GROUP BY yearID) tmg
    ON tmg.yearID = sb.yearID
WHERE sb.PA >= 3.1 * tmg.maxG
  AND la.lgOBP > 0 AND la.lgSLG > 0
ORDER BY OPS_plus DESC;

-- ============================================================
-- View 16: v_season_pitching_qualified
-- Purpose: Single-season pitching lines for qualified starters
--          (IP >= team G, i.e., IPouts >= 3 * team G).
-- Key formulas: ERA, WHIP, K/9, BB/9
-- Qualifiers: IPouts >= 3 * team G (1 IP per team game)
-- Notes: Aggregates across stints within yearID.
-- ============================================================
CREATE OR REPLACE VIEW v_season_pitching_qualified AS
WITH season_pitch AS (
    SELECT
        pi.playerID, pi.yearID,
        GROUP_CONCAT(DISTINCT pi.teamID ORDER BY pi.stint) AS teams,
        SUM(pi.W) AS W, SUM(pi.L) AS L,
        SUM(pi.G) AS G, SUM(pi.GS) AS GS,
        SUM(pi.CG) AS CG, SUM(pi.SHO) AS SHO, SUM(pi.SV) AS SV,
        SUM(pi.IPouts) AS IPouts,
        SUM(pi.H) AS H, SUM(pi.ER) AS ER, SUM(pi.HR) AS HR,
        SUM(COALESCE(pi.BB,0)) AS BB, SUM(pi.SO) AS SO,
        SUM(COALESCE(pi.HBP,0)) AS HBP,
        SUM(COALESCE(pi.BFP,0)) AS BFP
    FROM pitching pi
    WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG
    FROM teams WHERE lgID IN ('AL','NL')
    GROUP BY yearID
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sp.yearID,
    sp.teams,
    sp.W, sp.L,
    ROUND(sp.W / NULLIF(sp.W + sp.L, 0), 3) AS W_pct,
    ROUND(9.0 * sp.ER / NULLIF(sp.IPouts/3.0, 0), 2) AS ERA,
    sp.G, sp.GS, sp.CG, sp.SHO, sp.SV,
    ROUND(sp.IPouts / 3.0, 1) AS IP,
    sp.H, sp.ER, sp.HR, sp.BB, sp.SO,
    ROUND((sp.BB + sp.H) / NULLIF(sp.IPouts/3.0, 0), 3) AS WHIP,
    ROUND(9.0 * sp.SO / NULLIF(sp.IPouts/3.0, 0), 2) AS K_per_9,
    ROUND(9.0 * sp.BB / NULLIF(sp.IPouts/3.0, 0), 2) AS BB_per_9
FROM season_pitch sp
JOIN people p ON p.playerID = sp.playerID
JOIN team_games tg ON tg.yearID = sp.yearID
WHERE sp.IPouts >= 3 * tg.maxG
ORDER BY sp.yearID DESC, ERA ASC;

-- ============================================================
-- View 17: v_season_pitching_advanced
-- Purpose: Single-season FIP, K%, BB%, HR/9, K-BB% for
--          qualified pitchers.
-- Key formulas: FIP = ((13*HR+3*(BB+HBP)-2*SO)/IP) + cFIP
-- Qualifiers: IPouts >= 3 * team G
-- Notes: cFIP ≈ 3.10 used as simplification.
-- ============================================================
CREATE OR REPLACE VIEW v_season_pitching_advanced AS
WITH season_pitch AS (
    SELECT
        pi.playerID, pi.yearID,
        SUM(pi.IPouts) AS IPouts,
        SUM(pi.H) AS H, SUM(pi.ER) AS ER, SUM(pi.HR) AS HR,
        SUM(COALESCE(pi.BB,0)) AS BB, SUM(pi.SO) AS SO,
        SUM(COALESCE(pi.HBP,0)) AS HBP,
        SUM(COALESCE(pi.BFP,0)) AS BFP
    FROM pitching pi
    WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG
    FROM teams WHERE lgID IN ('AL','NL')
    GROUP BY yearID
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sp.yearID,
    ROUND(sp.IPouts/3.0, 1) AS IP,
    /* FIP */
    ROUND(
        (13.0*sp.HR + 3.0*(sp.BB+sp.HBP) - 2.0*sp.SO)
        / NULLIF(sp.IPouts/3.0, 0) + 3.10,
        2
    ) AS FIP,
    /* K% */
    ROUND(sp.SO / NULLIF(sp.BFP, 0), 3) AS K_pct,
    /* BB% */
    ROUND(sp.BB / NULLIF(sp.BFP, 0), 3) AS BB_pct,
    /* K-BB% */
    ROUND((sp.SO - sp.BB) / NULLIF(sp.BFP, 0), 3) AS K_BB_pct,
    /* HR/9 */
    ROUND(9.0*sp.HR / NULLIF(sp.IPouts/3.0, 0), 2) AS HR_per_9,
    /* BABIP */
    ROUND(
        (sp.H - sp.HR)
        / NULLIF(sp.BFP - sp.BB - sp.HBP - sp.SO - sp.HR, 0), 3
    ) AS BABIP
FROM season_pitch sp
JOIN people p ON p.playerID = sp.playerID
JOIN team_games tg ON tg.yearID = sp.yearID
WHERE sp.IPouts >= 3 * tg.maxG
ORDER BY FIP ASC;

-- ============================================================
-- View 18: v_season_pitching_era_plus
-- Purpose: Single-season ERA+ for qualified pitchers with
--          league context.
-- Key formulas: ERA+ = 100 * (lgERA / playerERA)
-- Qualifiers: IPouts >= 3 * team G
-- Notes: PPF (pitcher park factor) from teams table applied
--        where available.
-- ============================================================
CREATE OR REPLACE VIEW v_season_pitching_era_plus AS
WITH season_pitch AS (
    SELECT
        pi.playerID, pi.yearID, pi.lgID,
        SUM(pi.IPouts) AS IPouts,
        SUM(pi.ER) AS ER
    FROM pitching pi
    WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID, pi.lgID
),
league_era AS (
    SELECT yearID, lgID,
        9.0 * SUM(ER) / NULLIF(SUM(IPouts)/3.0, 0) AS lgERA
    FROM season_pitch
    GROUP BY yearID, lgID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG
    FROM teams WHERE lgID IN ('AL','NL')
    GROUP BY yearID
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sp.yearID,
    ROUND(sp.IPouts/3.0, 1) AS IP,
    ROUND(9.0*sp.ER / NULLIF(sp.IPouts/3.0, 0), 2) AS ERA,
    ROUND(le.lgERA, 2) AS lgERA,
    ROUND(
        CASE WHEN sp.ER > 0 AND sp.IPouts > 0
             THEN 100.0 * le.lgERA / (9.0*sp.ER / (sp.IPouts/3.0))
             ELSE NULL
        END, 1
    ) AS ERA_plus,
    RANK() OVER (PARTITION BY sp.yearID ORDER BY
        CASE WHEN sp.ER > 0 AND sp.IPouts > 0
             THEN 100.0 * le.lgERA / (9.0*sp.ER / (sp.IPouts/3.0))
             ELSE NULL
        END DESC
    ) AS year_rank
FROM season_pitch sp
JOIN league_era le ON le.yearID = sp.yearID AND le.lgID = sp.lgID
JOIN people p ON p.playerID = sp.playerID
JOIN team_games tg ON tg.yearID = sp.yearID
WHERE sp.IPouts >= 3 * tg.maxG
  AND sp.ER > 0
ORDER BY ERA_plus DESC;
-- ============================================================
-- View 19: v_season_batting_leaders_top100
-- Purpose: Top 100 single seasons ever by OPS+ (qualified).
-- Key formulas: OPS+ = 100 * (OBP/lgOBP + SLG/lgSLG - 1)
-- Qualifiers: PA >= 3.1 * team G
-- Notes: Ranks all-time greatest offensive single seasons.
-- ============================================================
CREATE OR REPLACE VIEW v_season_batting_leaders_top100 AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID, b.lgID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
),
league_avg AS (
    SELECT yearID, lgID,
        (SUM(H)+SUM(BB)+SUM(HBP)) / NULLIF(SUM(AB)+SUM(BB)+SUM(HBP)+SUM(SF),0) AS lgOBP,
        (SUM(H)+SUM(`2B`)+2*SUM(`3B`)+3*SUM(HR)) / NULLIF(SUM(AB),0) AS lgSLG
    FROM season_bat GROUP BY yearID, lgID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG FROM teams WHERE lgID IN ('AL','NL') GROUP BY yearID
),
ranked AS (
    SELECT
        p.playerID, p.nameFirst, p.nameLast,
        sb.yearID,
        sb.PA,
        ROUND(sb.H / NULLIF(sb.AB,0), 3) AS AVG,
        ROUND((sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0), 3) AS OBP,
        ROUND((sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0), 3) AS SLG,
        ROUND(
            (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0)
            + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0), 3
        ) AS OPS,
        ROUND(
            100 * (
                (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0) / la.lgOBP
              + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0) / la.lgSLG
              - 1
            ), 1
        ) AS OPS_plus,
        CASE
            WHEN sb.yearID BETWEEN 1901 AND 1919 THEN 'Dead Ball'
            WHEN sb.yearID BETWEEN 1920 AND 1941 THEN 'Live Ball'
            WHEN sb.yearID BETWEEN 1942 AND 1960 THEN 'Integration'
            WHEN sb.yearID BETWEEN 1961 AND 1976 THEN 'Expansion'
            WHEN sb.yearID BETWEEN 1977 AND 1993 THEN 'Free Agency'
            WHEN sb.yearID BETWEEN 1994 AND 2005 THEN 'Steroid'
            WHEN sb.yearID BETWEEN 2006 AND 2015 THEN 'Post-Steroid'
            WHEN sb.yearID >= 2016                THEN 'Statcast/Analytics'
            ELSE 'Pre-Modern'
        END AS era_name,
        ROW_NUMBER() OVER (ORDER BY
            100 * (
                (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0) / la.lgOBP
              + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0) / la.lgSLG
              - 1
            ) DESC
        ) AS alltime_rank
    FROM season_bat sb
    JOIN league_avg la ON la.yearID = sb.yearID AND la.lgID = sb.lgID
    JOIN people p ON p.playerID = sb.playerID
    JOIN team_games tg ON tg.yearID = sb.yearID
    WHERE sb.PA >= 3.1 * tg.maxG
      AND la.lgOBP > 0 AND la.lgSLG > 0
)
SELECT * FROM ranked WHERE alltime_rank <= 100
ORDER BY alltime_rank;

-- ============================================================
-- View 20: v_season_pitching_leaders_top100
-- Purpose: Top 100 single seasons ever by ERA+ (qualified).
-- Key formulas: ERA+ = 100 * (lgERA / playerERA)
-- Qualifiers: IPouts >= 3 * team G
-- Notes: Ranks all-time greatest pitching single seasons.
-- ============================================================
CREATE OR REPLACE VIEW v_season_pitching_leaders_top100 AS
WITH season_pitch AS (
    SELECT
        pi.playerID, pi.yearID, pi.lgID,
        SUM(pi.IPouts) AS IPouts, SUM(pi.ER) AS ER,
        SUM(pi.W) AS W, SUM(pi.L) AS L,
        SUM(pi.SO) AS SO, SUM(pi.H) AS H,
        SUM(COALESCE(pi.BB,0)) AS BB
    FROM pitching pi WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID, pi.lgID
),
league_era AS (
    SELECT yearID, lgID,
        9.0*SUM(ER)/NULLIF(SUM(IPouts)/3.0,0) AS lgERA
    FROM season_pitch GROUP BY yearID, lgID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG FROM teams WHERE lgID IN ('AL','NL') GROUP BY yearID
),
ranked AS (
    SELECT
        p.playerID, p.nameFirst, p.nameLast,
        sp.yearID,
        sp.W, sp.L,
        ROUND(sp.IPouts/3.0,1) AS IP,
        ROUND(9.0*sp.ER/NULLIF(sp.IPouts/3.0,0), 2) AS ERA,
        ROUND(le.lgERA, 2) AS lgERA,
        sp.SO,
        ROUND((sp.BB+sp.H)/NULLIF(sp.IPouts/3.0,0), 3) AS WHIP,
        ROUND(
            CASE WHEN sp.ER > 0 AND sp.IPouts > 0
                 THEN 100.0 * le.lgERA / (9.0*sp.ER/(sp.IPouts/3.0))
                 ELSE NULL END, 1
        ) AS ERA_plus,
        CASE
            WHEN sp.yearID BETWEEN 1901 AND 1919 THEN 'Dead Ball'
            WHEN sp.yearID BETWEEN 1920 AND 1941 THEN 'Live Ball'
            WHEN sp.yearID BETWEEN 1942 AND 1960 THEN 'Integration'
            WHEN sp.yearID BETWEEN 1961 AND 1976 THEN 'Expansion'
            WHEN sp.yearID BETWEEN 1977 AND 1993 THEN 'Free Agency'
            WHEN sp.yearID BETWEEN 1994 AND 2005 THEN 'Steroid'
            WHEN sp.yearID BETWEEN 2006 AND 2015 THEN 'Post-Steroid'
            WHEN sp.yearID >= 2016                THEN 'Statcast/Analytics'
            ELSE 'Pre-Modern'
        END AS era_name,
        ROW_NUMBER() OVER (ORDER BY
            CASE WHEN sp.ER > 0 AND sp.IPouts > 0
                 THEN 100.0 * le.lgERA / (9.0*sp.ER/(sp.IPouts/3.0))
                 ELSE NULL END DESC
        ) AS alltime_rank
    FROM season_pitch sp
    JOIN league_era le ON le.yearID = sp.yearID AND le.lgID = sp.lgID
    JOIN people p ON p.playerID = sp.playerID
    JOIN team_games tg ON tg.yearID = sp.yearID
    WHERE sp.IPouts >= 3 * tg.maxG AND sp.ER > 0
)
SELECT * FROM ranked WHERE alltime_rank <= 100
ORDER BY alltime_rank;

-- ============================================================
-- View 21: v_season_batting_broadcast_statline
-- Purpose: Broadcast-ready format: Player | Team | Year |
--          AVG | HR | RBI | OPS | All-Star? | MVP?
-- Key formulas: Standard rate stats.
-- Qualifiers: AB >= 200 (any semi-regular player)
-- Notes: Joins to awardsplayers and allstarfull for flags.
-- ============================================================
CREATE OR REPLACE VIEW v_season_batting_broadcast_statline AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID,
        GROUP_CONCAT(DISTINCT b.teamID ORDER BY b.stint) AS team,
        SUM(b.AB) AS AB, SUM(b.H) AS H, SUM(b.HR) AS HR,
        SUM(COALESCE(b.RBI,0)) AS RBI,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID
),
allstars AS (
    SELECT DISTINCT playerID, yearID FROM allstarfull WHERE lgID IN ('AL','NL')
),
mvps AS (
    SELECT DISTINCT playerID, yearID
    FROM awardsplayers
    WHERE awardID = 'Most Valuable Player'
)
SELECT
    CONCAT(p.nameFirst, ' ', p.nameLast) AS player_name,
    sb.team,
    sb.yearID AS season,
    ROUND(sb.H / NULLIF(sb.AB,0), 3) AS AVG,
    sb.HR,
    sb.RBI,
    ROUND(
        (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0)
        + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0), 3
    ) AS OPS,
    CASE WHEN ast.playerID IS NOT NULL THEN 'Yes' ELSE 'No' END AS all_star,
    CASE WHEN m.playerID IS NOT NULL THEN 'Yes' ELSE 'No' END AS MVP
FROM season_bat sb
JOIN people p ON p.playerID = sb.playerID
LEFT JOIN allstars ast ON ast.playerID = sb.playerID AND ast.yearID = sb.yearID
LEFT JOIN mvps m ON m.playerID = sb.playerID AND m.yearID = sb.yearID
WHERE sb.AB >= 200
ORDER BY sb.yearID DESC, OPS DESC;

-- ============================================================
-- View 22: v_season_pitching_broadcast_statline
-- Purpose: Broadcast-ready: Player | Team | Year | W-L | ERA
--          | SO | WHIP | Cy Young Award?
-- Key formulas: ERA, WHIP
-- Qualifiers: IPouts >= 150 (50+ IP)
-- Notes: Joins to awardsplayers for CYA flag.
-- ============================================================
CREATE OR REPLACE VIEW v_season_pitching_broadcast_statline AS
WITH season_pitch AS (
    SELECT
        pi.playerID, pi.yearID,
        GROUP_CONCAT(DISTINCT pi.teamID ORDER BY pi.stint) AS team,
        SUM(pi.W) AS W, SUM(pi.L) AS L,
        SUM(pi.IPouts) AS IPouts,
        SUM(pi.ER) AS ER, SUM(pi.H) AS H,
        SUM(COALESCE(pi.BB,0)) AS BB, SUM(pi.SO) AS SO
    FROM pitching pi WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID
),
cya AS (
    SELECT DISTINCT playerID, yearID
    FROM awardsplayers
    WHERE awardID = 'Cy Young Award'
)
SELECT
    CONCAT(p.nameFirst, ' ', p.nameLast) AS player_name,
    sp.team,
    sp.yearID AS season,
    CONCAT(sp.W, '-', sp.L) AS W_L,
    ROUND(9.0*sp.ER / NULLIF(sp.IPouts/3.0, 0), 2) AS ERA,
    sp.SO,
    ROUND((sp.BB + sp.H) / NULLIF(sp.IPouts/3.0, 0), 3) AS WHIP,
    ROUND(sp.IPouts/3.0, 1) AS IP,
    CASE WHEN c.playerID IS NOT NULL THEN 'Yes' ELSE 'No' END AS Cy_Young
FROM season_pitch sp
JOIN people p ON p.playerID = sp.playerID
LEFT JOIN cya c ON c.playerID = sp.playerID AND c.yearID = sp.yearID
WHERE sp.IPouts >= 150
ORDER BY sp.yearID DESC, ERA ASC;

-- ============================================================
-- View 23: v_season_hr_leaders_by_year
-- Purpose: HR leader for each year with rank within year.
-- Key formulas: SUM(HR) per player per year.
-- Qualifiers: None
-- Notes: Shows top 10 per year via DENSE_RANK.
-- ============================================================
CREATE OR REPLACE VIEW v_season_hr_leaders_by_year AS
WITH season_hr AS (
    SELECT
        b.playerID, b.yearID,
        SUM(b.HR) AS HR,
        DENSE_RANK() OVER (PARTITION BY b.yearID ORDER BY SUM(b.HR) DESC) AS hr_rank
    FROM batting b
    WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID
    HAVING SUM(b.HR) > 0
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sh.yearID,
    sh.HR,
    sh.hr_rank
FROM season_hr sh
JOIN people p ON p.playerID = sh.playerID
WHERE sh.hr_rank <= 10
ORDER BY sh.yearID DESC, sh.hr_rank ASC;

-- ============================================================
-- View 24: v_season_batting_woba_leaders
-- Purpose: Top wOBA seasons, >= 400 PA.
-- Key formulas: wOBA linear weights (see header).
-- Qualifiers: PA >= 400
-- Notes: Ranked all-time by wOBA.
-- ============================================================
CREATE OR REPLACE VIEW v_season_batting_woba_leaders AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.IBB,0)) AS IBB,
        SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sb.yearID,
    sb.PA,
    ROUND(
        (0.69*(sb.BB - sb.IBB) + 0.72*sb.HBP
         + 0.87*(sb.H - sb.`2B` - sb.`3B` - sb.HR)
         + 1.22*sb.`2B` + 1.56*sb.`3B` + 1.95*sb.HR)
        / NULLIF(sb.AB + sb.BB - sb.IBB + sb.SF + sb.HBP, 0), 3
    ) AS wOBA,
    RANK() OVER (ORDER BY
        (0.69*(sb.BB - sb.IBB) + 0.72*sb.HBP
         + 0.87*(sb.H - sb.`2B` - sb.`3B` - sb.HR)
         + 1.22*sb.`2B` + 1.56*sb.`3B` + 1.95*sb.HR)
        / NULLIF(sb.AB + sb.BB - sb.IBB + sb.SF + sb.HBP, 0)
        DESC
    ) AS woba_rank
FROM season_bat sb
JOIN people p ON p.playerID = sb.playerID
WHERE sb.PA >= 400
ORDER BY wOBA DESC;
-- ============================================================
-- View 25: v_team_season_offense
-- Purpose: Yearly team offense: R, HR, AVG, OBP, SLG, OPS, SB.
-- Key formulas: Standard rate stats at team level.
-- Qualifiers: AL/NL teams only
-- Notes: Uses the teams table directly for most aggregates.
-- ============================================================
CREATE OR REPLACE VIEW v_team_season_offense AS
SELECT
    t.yearID,
    t.teamID,
    t.lgID,
    t.name AS team_name,
    t.G,
    t.W,
    t.L,
    t.R,
    t.AB,
    t.H,
    t.`2B`,
    t.`3B`,
    t.HR,
    t.BB,
    t.SO,
    COALESCE(t.SB,0)                                       AS SB,
    COALESCE(t.CS,0)                                       AS CS,
    ROUND(t.H / NULLIF(t.AB, 0), 3)                        AS AVG,
    ROUND(
        (t.H + t.BB + COALESCE(t.HBP,0))
        / NULLIF(t.AB + t.BB + COALESCE(t.HBP,0) + COALESCE(t.SF,0), 0), 3
    )                                                      AS OBP,
    ROUND(
        (t.H + t.`2B` + 2*t.`3B` + 3*t.HR)
        / NULLIF(t.AB, 0), 3
    )                                                      AS SLG,
    ROUND(
        (t.H + t.BB + COALESCE(t.HBP,0))
        / NULLIF(t.AB + t.BB + COALESCE(t.HBP,0) + COALESCE(t.SF,0), 0)
        + (t.H + t.`2B` + 2*t.`3B` + 3*t.HR) / NULLIF(t.AB, 0), 3
    )                                                      AS OPS,
    ROUND(t.R / NULLIF(t.G, 0), 2)                         AS R_per_G
FROM teams t
WHERE t.lgID IN ('AL','NL')
ORDER BY t.yearID DESC, OPS DESC;

-- ============================================================
-- View 26: v_team_season_pitching
-- Purpose: Yearly team pitching: ERA, WHIP, K/9, BB/9, SV,
--          CG, SHO, FIP estimate.
-- Key formulas: ERA from teams table, WHIP=(BBA+HA)/IP,
--   FIP = (13*HRA + 3*BBA - 2*SOA)/(IPouts/3) + cFIP
-- Qualifiers: AL/NL teams only
-- Notes: Uses teams-level allowed stats (HA, HRA, BBA, SOA).
--        cFIP ≈ 3.10 as constant approximation.
-- ============================================================
CREATE OR REPLACE VIEW v_team_season_pitching AS
SELECT
    t.yearID,
    t.teamID,
    t.lgID,
    t.name AS team_name,
    t.G,
    t.W,
    t.L,
    t.ERA,
    ROUND(t.IPouts / 3.0, 1)                              AS IP,
    ROUND(
        (COALESCE(t.BBA,0) + COALESCE(t.HA,0))
        / NULLIF(t.IPouts/3.0, 0), 3
    )                                                      AS WHIP,
    ROUND(9.0 * COALESCE(t.SOA,0) / NULLIF(t.IPouts/3.0, 0), 2) AS K_per_9,
    ROUND(9.0 * COALESCE(t.BBA,0) / NULLIF(t.IPouts/3.0, 0), 2) AS BB_per_9,
    t.SV,
    t.CG,
    t.SHO,
    t.RA,
    t.ER,
    /* Team FIP estimate */
    ROUND(
        (13.0*COALESCE(t.HRA,0) + 3.0*COALESCE(t.BBA,0) - 2.0*COALESCE(t.SOA,0))
        / NULLIF(t.IPouts/3.0, 0) + 3.10,
        2
    )                                                      AS team_FIP
FROM teams t
WHERE t.lgID IN ('AL','NL')
ORDER BY t.yearID DESC, t.ERA ASC;

-- ============================================================
-- View 27: v_team_season_run_differential
-- Purpose: Run differential, Pythagorean W%, actual W%, and
--          luck factor (actual W% - expected W%).
-- Key formulas: Pyth W% = R^2 / (R^2 + RA^2)
--   (Basic Pythagorean with exponent 2; Bill James formula)
--   Luck = actual_W_pct - pyth_W_pct
-- Qualifiers: AL/NL teams only
-- Notes: Pythagorean exponent of 2 is classic; 1.83 is more
--        accurate (Pythagenpat) but 2 is traditional.
-- ============================================================
CREATE OR REPLACE VIEW v_team_season_run_differential AS
SELECT
    t.yearID,
    t.teamID,
    t.lgID,
    t.name AS team_name,
    t.G,
    t.W,
    t.L,
    ROUND(t.W / NULLIF(t.W + t.L, 0), 3)                 AS W_pct,
    t.R,
    t.RA,
    (t.R - t.RA)                                           AS run_diff,
    ROUND(
        POWER(t.R, 2) / NULLIF(POWER(t.R, 2) + POWER(t.RA, 2), 0), 3
    )                                                      AS pyth_W_pct,
    ROUND(
        POWER(t.R, 2) / NULLIF(POWER(t.R, 2) + POWER(t.RA, 2), 0) * (t.W + t.L), 0
    )                                                      AS pyth_W,
    ROUND(
        t.W / NULLIF(t.W + t.L, 0)
        - POWER(t.R, 2) / NULLIF(POWER(t.R, 2) + POWER(t.RA, 2), 0),
        3
    )                                                      AS luck_factor,
    t.DivWin, t.WCWin, t.LgWin, t.WSWin
FROM teams t
WHERE t.lgID IN ('AL','NL')
ORDER BY t.yearID DESC, run_diff DESC;

-- ============================================================
-- View 28: v_team_franchise_alltime
-- Purpose: All-time franchise totals: years, W, L, W%,
--          pennants (LgWin), WS wins (WSWin).
-- Key formulas: Simple aggregation by franchID.
-- Qualifiers: AL/NL only
-- Notes: Joins to teamsfranchises for franchise name.
-- ============================================================
CREATE OR REPLACE VIEW v_team_franchise_alltime AS
SELECT
    t.franchID,
    tf.franchName,
    tf.active,
    COUNT(DISTINCT t.yearID)                               AS seasons,
    SUM(t.W)                                               AS total_W,
    SUM(t.L)                                               AS total_L,
    ROUND(SUM(t.W) / NULLIF(SUM(t.W)+SUM(t.L), 0), 3)    AS alltime_W_pct,
    SUM(CASE WHEN t.LgWin = 'Y' THEN 1 ELSE 0 END)       AS pennants,
    SUM(CASE WHEN t.WSWin = 'Y' THEN 1 ELSE 0 END)        AS ws_titles,
    SUM(CASE WHEN t.DivWin = 'Y' THEN 1 ELSE 0 END)       AS div_titles,
    SUM(t.R)                                               AS total_R,
    SUM(t.RA)                                              AS total_RA,
    ROUND(SUM(t.attendance) / NULLIF(COUNT(DISTINCT t.yearID), 0), 0) AS avg_attendance
FROM teams t
JOIN teamsfranchises tf ON tf.franchID = t.franchID
WHERE t.lgID IN ('AL','NL')
GROUP BY t.franchID, tf.franchName, tf.active
ORDER BY ws_titles DESC, alltime_W_pct DESC;

-- ============================================================
-- View 29: v_team_dynasty_score
-- Purpose: Multi-year dynasty rating: rolling 5-year W%,
--          postseason success.
-- Key formulas: 5-year rolling W%, WS wins in window,
--   pennants in window.
--   DynastyScore = 50*(5yr_Wpct) + 30*(WS_wins/5) + 20*(pennants/5)
-- Qualifiers: AL/NL only, yearID >= 1905 (for 5-year window)
-- Notes: Uses window functions to compute rolling sums.
-- ============================================================
CREATE OR REPLACE VIEW v_team_dynasty_score AS
WITH team_years AS (
    SELECT
        t.yearID,
        t.franchID,
        tf.franchName,
        t.W,
        t.L,
        CASE WHEN t.WSWin = 'Y' THEN 1 ELSE 0 END AS ws_win,
        CASE WHEN t.LgWin = 'Y' THEN 1 ELSE 0 END AS pennant,
        CASE WHEN t.DivWin = 'Y' OR t.WCWin = 'Y' THEN 1 ELSE 0 END AS playoff
    FROM teams t
    JOIN teamsfranchises tf ON tf.franchID = t.franchID
    WHERE t.lgID IN ('AL','NL')
)
SELECT
    ty.yearID AS window_end_year,
    ty.franchID,
    ty.franchName,
    SUM(ty.W) OVER w                                       AS five_yr_W,
    SUM(ty.L) OVER w                                       AS five_yr_L,
    ROUND(
        SUM(ty.W) OVER w
        / NULLIF(SUM(ty.W) OVER w + SUM(ty.L) OVER w, 0), 3
    )                                                      AS five_yr_W_pct,
    SUM(ty.ws_win) OVER w                                  AS five_yr_WS,
    SUM(ty.pennant) OVER w                                 AS five_yr_pennants,
    SUM(ty.playoff) OVER w                                 AS five_yr_playoffs,
    ROUND(
        50.0 * SUM(ty.W) OVER w / NULLIF(SUM(ty.W) OVER w + SUM(ty.L) OVER w, 0)
      + 30.0 * SUM(ty.ws_win) OVER w / 5.0
      + 20.0 * SUM(ty.pennant) OVER w / 5.0,
        1
    )                                                      AS dynasty_score
FROM team_years ty
WINDOW w AS (PARTITION BY ty.franchID ORDER BY ty.yearID
             ROWS BETWEEN 4 PRECEDING AND CURRENT ROW)
ORDER BY dynasty_score DESC;

-- ============================================================
-- View 30: v_team_payroll_efficiency
-- Purpose: Salary vs wins for years with salary data.
-- Key formulas: total_payroll, cost_per_win = payroll / W,
--   payroll rank per year.
-- Qualifiers: Years with salary data in salaries table
-- Notes: Salary data available ~1985-2016 in Lahman.
-- ============================================================
CREATE OR REPLACE VIEW v_team_payroll_efficiency AS
WITH team_payroll AS (
    SELECT
        s.yearID,
        s.teamID,
        s.lgID,
        SUM(s.salary) AS total_payroll
    FROM salaries s
    WHERE s.lgID IN ('AL','NL')
    GROUP BY s.yearID, s.teamID, s.lgID
)
SELECT
    tp.yearID,
    tp.teamID,
    tp.lgID,
    t.name AS team_name,
    t.W,
    t.L,
    ROUND(t.W / NULLIF(t.W+t.L, 0), 3) AS W_pct,
    ROUND(tp.total_payroll, 0) AS total_payroll,
    ROUND(tp.total_payroll / NULLIF(t.W, 0), 0) AS cost_per_win,
    RANK() OVER (PARTITION BY tp.yearID ORDER BY tp.total_payroll DESC) AS payroll_rank,
    RANK() OVER (PARTITION BY tp.yearID ORDER BY t.W DESC) AS wins_rank,
    t.DivWin, t.WCWin, t.LgWin, t.WSWin
FROM team_payroll tp
JOIN teams t ON t.yearID = tp.yearID AND t.teamID = tp.teamID AND t.lgID = tp.lgID
ORDER BY tp.yearID DESC, total_payroll DESC;

-- ============================================================
-- View 31: v_team_season_rankings
-- Purpose: Teams ranked by W% within each year, with playoff
--          results.
-- Key formulas: W%, RANK within year.
-- Qualifiers: AL/NL only
-- Notes: Includes DivWin, WCWin, LgWin, WSWin flags.
-- ============================================================
CREATE OR REPLACE VIEW v_team_season_rankings AS
SELECT
    t.yearID,
    t.teamID,
    t.lgID,
    t.name AS team_name,
    t.W,
    t.L,
    ROUND(t.W / NULLIF(t.W+t.L, 0), 3) AS W_pct,
    RANK() OVER (PARTITION BY t.yearID ORDER BY t.W / NULLIF(t.W+t.L, 0) DESC) AS overall_rank,
    RANK() OVER (PARTITION BY t.yearID, t.lgID ORDER BY t.W / NULLIF(t.W+t.L, 0) DESC) AS league_rank,
    t.DivWin,
    t.WCWin,
    t.LgWin,
    t.WSWin,
    t.R,
    t.RA,
    (t.R - t.RA) AS run_diff,
    t.attendance
FROM teams t
WHERE t.lgID IN ('AL','NL')
ORDER BY t.yearID DESC, W_pct DESC;

-- ============================================================
-- View 32: v_team_best_seasons_ever
-- Purpose: Top 50 team-seasons by W%, with run differential
--          and postseason result.
-- Key formulas: W%, run differential.
-- Qualifiers: AL/NL only, G >= 100 (exclude shortened seasons
--             below 100 games)
-- Notes: Ranked by W% all-time.
-- ============================================================
CREATE OR REPLACE VIEW v_team_best_seasons_ever AS
WITH ranked_teams AS (
    SELECT
        t.yearID,
        t.teamID,
        t.lgID,
        t.name AS team_name,
        t.W,
        t.L,
        t.G,
        ROUND(t.W / NULLIF(t.W+t.L, 0), 3)               AS W_pct,
        t.R,
        t.RA,
        (t.R - t.RA)                                       AS run_diff,
        t.DivWin,
        t.LgWin,
        t.WSWin,
        ROW_NUMBER() OVER (ORDER BY t.W / NULLIF(t.W+t.L, 0) DESC) AS alltime_rank
    FROM teams t
    WHERE t.lgID IN ('AL','NL')
      AND (t.W + t.L) >= 100
)
SELECT * FROM ranked_teams
WHERE alltime_rank <= 50
ORDER BY alltime_rank;
-- ============================================================
-- View 33: v_era_batting_environment
-- Purpose: League-wide batting stats by year: AVG, OBP, SLG,
--          HR/G, K/G, BB/G for AL+NL combined.
-- Key formulas: Standard rate stats at league level.
-- Qualifiers: yearID >= 1901 (modern era), AL/NL only
-- Notes: Shows how the run environment changed over time.
--        Useful as a baseline for era-adjusted comparisons.
-- ============================================================
CREATE OR REPLACE VIEW v_era_batting_environment AS
SELECT
    t.yearID,
    CASE
        WHEN t.yearID BETWEEN 1901 AND 1919 THEN 'Dead Ball'
        WHEN t.yearID BETWEEN 1920 AND 1941 THEN 'Live Ball'
        WHEN t.yearID BETWEEN 1942 AND 1960 THEN 'Integration'
        WHEN t.yearID BETWEEN 1961 AND 1976 THEN 'Expansion'
        WHEN t.yearID BETWEEN 1977 AND 1993 THEN 'Free Agency'
        WHEN t.yearID BETWEEN 1994 AND 2005 THEN 'Steroid'
        WHEN t.yearID BETWEEN 2006 AND 2015 THEN 'Post-Steroid'
        WHEN t.yearID >= 2016                THEN 'Statcast/Analytics'
        ELSE 'Pre-Modern'
    END                                                     AS era_name,
    COUNT(DISTINCT t.teamID)                                AS num_teams,
    SUM(t.G)                                                AS total_G,
    ROUND(SUM(t.H) / NULLIF(SUM(t.AB),0), 3)               AS lg_AVG,
    ROUND(
        (SUM(t.H) + SUM(t.BB) + SUM(COALESCE(t.HBP,0)))
        / NULLIF(SUM(t.AB) + SUM(t.BB) + SUM(COALESCE(t.HBP,0)) + SUM(COALESCE(t.SF,0)), 0), 3
    )                                                       AS lg_OBP,
    ROUND(
        (SUM(t.H) + SUM(t.`2B`) + 2*SUM(t.`3B`) + 3*SUM(t.HR))
        / NULLIF(SUM(t.AB), 0), 3
    )                                                       AS lg_SLG,
    ROUND(
        (SUM(t.H) + SUM(t.BB) + SUM(COALESCE(t.HBP,0)))
        / NULLIF(SUM(t.AB) + SUM(t.BB) + SUM(COALESCE(t.HBP,0)) + SUM(COALESCE(t.SF,0)), 0)
        + (SUM(t.H) + SUM(t.`2B`) + 2*SUM(t.`3B`) + 3*SUM(t.HR)) / NULLIF(SUM(t.AB), 0),
        3
    )                                                       AS lg_OPS,
    ROUND(SUM(t.R) / NULLIF(SUM(t.G), 0), 2)               AS R_per_G,
    ROUND(SUM(t.HR) / NULLIF(SUM(t.G), 0), 2)              AS HR_per_G,
    ROUND(SUM(COALESCE(t.SO,0)) / NULLIF(SUM(t.G), 0), 2)  AS K_per_G,
    ROUND(SUM(t.BB) / NULLIF(SUM(t.G), 0), 2)              AS BB_per_G
FROM teams t
WHERE t.lgID IN ('AL','NL')
  AND t.yearID >= 1901
GROUP BY t.yearID
ORDER BY t.yearID;

-- ============================================================
-- View 34: v_era_pitching_environment
-- Purpose: League-wide pitching stats by year: ERA, WHIP,
--          K/9, HR/9 for AL+NL combined.
-- Key formulas: ERA, WHIP, K/9, HR/9 at league level.
-- Qualifiers: yearID >= 1901, AL/NL only
-- Notes: Complement to view 33 for the pitching side.
-- ============================================================
CREATE OR REPLACE VIEW v_era_pitching_environment AS
SELECT
    t.yearID,
    CASE
        WHEN t.yearID BETWEEN 1901 AND 1919 THEN 'Dead Ball'
        WHEN t.yearID BETWEEN 1920 AND 1941 THEN 'Live Ball'
        WHEN t.yearID BETWEEN 1942 AND 1960 THEN 'Integration'
        WHEN t.yearID BETWEEN 1961 AND 1976 THEN 'Expansion'
        WHEN t.yearID BETWEEN 1977 AND 1993 THEN 'Free Agency'
        WHEN t.yearID BETWEEN 1994 AND 2005 THEN 'Steroid'
        WHEN t.yearID BETWEEN 2006 AND 2015 THEN 'Post-Steroid'
        WHEN t.yearID >= 2016                THEN 'Statcast/Analytics'
        ELSE 'Pre-Modern'
    END                                                     AS era_name,
    ROUND(SUM(t.ER) * 9.0 / NULLIF(SUM(t.IPouts)/3.0, 0), 2) AS lg_ERA,
    ROUND(
        (SUM(COALESCE(t.BBA,0)) + SUM(COALESCE(t.HA,0)))
        / NULLIF(SUM(t.IPouts)/3.0, 0), 3
    )                                                       AS lg_WHIP,
    ROUND(9.0 * SUM(COALESCE(t.SOA,0)) / NULLIF(SUM(t.IPouts)/3.0, 0), 2) AS lg_K_per_9,
    ROUND(9.0 * SUM(COALESCE(t.BBA,0)) / NULLIF(SUM(t.IPouts)/3.0, 0), 2) AS lg_BB_per_9,
    ROUND(9.0 * SUM(COALESCE(t.HRA,0)) / NULLIF(SUM(t.IPouts)/3.0, 0), 2) AS lg_HR_per_9,
    SUM(t.CG)                                               AS lg_CG,
    SUM(t.SHO)                                              AS lg_SHO,
    SUM(t.SV)                                               AS lg_SV,
    ROUND(SUM(t.CG) / NULLIF(SUM(t.G), 0), 3)              AS CG_per_team_G
FROM teams t
WHERE t.lgID IN ('AL','NL')
  AND t.yearID >= 1901
GROUP BY t.yearID
ORDER BY t.yearID;

-- ============================================================
-- View 35: v_era_adjusted_batting_comparison
-- Purpose: Top 50 all-time seasons by era-adjusted OPS (OPS+),
--          showing raw and adjusted side by side.
-- Key formulas: OPS+ = 100*(OBP/lgOBP + SLG/lgSLG - 1)
-- Qualifiers: PA >= 3.1 * team G
-- Notes: This view reuses the OPS+ logic from view 19 but
--        limits to top 50 and adds raw vs adjusted columns.
-- ============================================================
CREATE OR REPLACE VIEW v_era_adjusted_batting_comparison AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID, b.lgID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
),
league_avg AS (
    SELECT yearID, lgID,
        (SUM(H)+SUM(BB)+SUM(HBP)) / NULLIF(SUM(AB)+SUM(BB)+SUM(HBP)+SUM(SF),0) AS lgOBP,
        (SUM(H)+SUM(`2B`)+2*SUM(`3B`)+3*SUM(HR)) / NULLIF(SUM(AB),0) AS lgSLG
    FROM season_bat GROUP BY yearID, lgID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG FROM teams WHERE lgID IN ('AL','NL') GROUP BY yearID
),
computed AS (
    SELECT
        p.playerID, p.nameFirst, p.nameLast,
        sb.yearID,
        CASE
            WHEN sb.yearID BETWEEN 1901 AND 1919 THEN 'Dead Ball'
            WHEN sb.yearID BETWEEN 1920 AND 1941 THEN 'Live Ball'
            WHEN sb.yearID BETWEEN 1942 AND 1960 THEN 'Integration'
            WHEN sb.yearID BETWEEN 1961 AND 1976 THEN 'Expansion'
            WHEN sb.yearID BETWEEN 1977 AND 1993 THEN 'Free Agency'
            WHEN sb.yearID BETWEEN 1994 AND 2005 THEN 'Steroid'
            WHEN sb.yearID BETWEEN 2006 AND 2015 THEN 'Post-Steroid'
            WHEN sb.yearID >= 2016                THEN 'Statcast/Analytics'
            ELSE 'Pre-Modern'
        END AS era_name,
        sb.PA,
        /* Raw stats */
        ROUND(sb.H / NULLIF(sb.AB,0), 3) AS raw_AVG,
        ROUND((sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0), 3) AS raw_OBP,
        ROUND((sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0), 3) AS raw_SLG,
        ROUND(
            (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0)
            + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0), 3
        ) AS raw_OPS,
        /* League context */
        ROUND(la.lgOBP, 3) AS lgOBP,
        ROUND(la.lgSLG, 3) AS lgSLG,
        /* OPS+ */
        ROUND(
            100 * (
                (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0) / la.lgOBP
              + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0) / la.lgSLG
              - 1
            ), 1
        ) AS OPS_plus,
        ROW_NUMBER() OVER (ORDER BY
            100 * (
                (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0) / la.lgOBP
              + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0) / la.lgSLG
              - 1
            ) DESC
        ) AS alltime_rank
    FROM season_bat sb
    JOIN league_avg la ON la.yearID = sb.yearID AND la.lgID = sb.lgID
    JOIN people p ON p.playerID = sb.playerID
    JOIN team_games tg ON tg.yearID = sb.yearID
    WHERE sb.PA >= 3.1 * tg.maxG AND la.lgOBP > 0 AND la.lgSLG > 0
)
SELECT * FROM computed WHERE alltime_rank <= 50
ORDER BY alltime_rank;

-- ============================================================
-- View 36: v_era_adjusted_pitching_comparison
-- Purpose: Top 50 all-time seasons by era-adjusted ERA (ERA+),
--          raw + adjusted side by side.
-- Key formulas: ERA+ = 100 * (lgERA / playerERA)
-- Qualifiers: IPouts >= 3 * team G
-- Notes: Shows how dominant seasons compare across eras.
-- ============================================================
CREATE OR REPLACE VIEW v_era_adjusted_pitching_comparison AS
WITH season_pitch AS (
    SELECT
        pi.playerID, pi.yearID, pi.lgID,
        SUM(pi.IPouts) AS IPouts, SUM(pi.ER) AS ER,
        SUM(pi.W) AS W, SUM(pi.L) AS L,
        SUM(pi.H) AS H, SUM(COALESCE(pi.BB,0)) AS BB,
        SUM(pi.SO) AS SO, SUM(pi.HR) AS HR
    FROM pitching pi WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID, pi.lgID
),
league_era AS (
    SELECT yearID, lgID,
        9.0*SUM(ER)/NULLIF(SUM(IPouts)/3.0,0) AS lgERA
    FROM season_pitch GROUP BY yearID, lgID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG FROM teams WHERE lgID IN ('AL','NL') GROUP BY yearID
),
computed AS (
    SELECT
        p.playerID, p.nameFirst, p.nameLast,
        sp.yearID,
        CASE
            WHEN sp.yearID BETWEEN 1901 AND 1919 THEN 'Dead Ball'
            WHEN sp.yearID BETWEEN 1920 AND 1941 THEN 'Live Ball'
            WHEN sp.yearID BETWEEN 1942 AND 1960 THEN 'Integration'
            WHEN sp.yearID BETWEEN 1961 AND 1976 THEN 'Expansion'
            WHEN sp.yearID BETWEEN 1977 AND 1993 THEN 'Free Agency'
            WHEN sp.yearID BETWEEN 1994 AND 2005 THEN 'Steroid'
            WHEN sp.yearID BETWEEN 2006 AND 2015 THEN 'Post-Steroid'
            WHEN sp.yearID >= 2016                THEN 'Statcast/Analytics'
            ELSE 'Pre-Modern'
        END AS era_name,
        sp.W, sp.L,
        ROUND(sp.IPouts/3.0, 1) AS IP,
        /* Raw stats */
        ROUND(9.0*sp.ER/NULLIF(sp.IPouts/3.0,0), 2) AS raw_ERA,
        ROUND((sp.BB+sp.H)/NULLIF(sp.IPouts/3.0,0), 3) AS raw_WHIP,
        sp.SO,
        /* League context */
        ROUND(le.lgERA, 2) AS lgERA,
        /* ERA+ */
        ROUND(
            CASE WHEN sp.ER > 0 AND sp.IPouts > 0
                 THEN 100.0 * le.lgERA / (9.0*sp.ER/(sp.IPouts/3.0))
                 ELSE NULL END, 1
        ) AS ERA_plus,
        ROW_NUMBER() OVER (ORDER BY
            CASE WHEN sp.ER > 0 AND sp.IPouts > 0
                 THEN 100.0 * le.lgERA / (9.0*sp.ER/(sp.IPouts/3.0))
                 ELSE NULL END DESC
        ) AS alltime_rank
    FROM season_pitch sp
    JOIN league_era le ON le.yearID = sp.yearID AND le.lgID = sp.lgID
    JOIN people p ON p.playerID = sp.playerID
    JOIN team_games tg ON tg.yearID = sp.yearID
    WHERE sp.IPouts >= 3 * tg.maxG AND sp.ER > 0
)
SELECT * FROM computed WHERE alltime_rank <= 50
ORDER BY alltime_rank;

-- ============================================================
-- View 37: v_era_peak_batting_5yr
-- Purpose: Best 5-year peak by OPS+ for each player with >= 5
--          qualified seasons.
-- Key formulas: OPS+ weighted by PA over best 5-year window.
-- Qualifiers: >= 5 qualified seasons (PA >= 3.1*teamG)
-- Notes: Uses a self-join to find the optimal 5-year window.
--        Window is rolling consecutive years where qualified.
-- ============================================================
CREATE OR REPLACE VIEW v_era_peak_batting_5yr AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID, b.lgID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
),
league_avg AS (
    SELECT yearID, lgID,
        (SUM(H)+SUM(BB)+SUM(HBP)) / NULLIF(SUM(AB)+SUM(BB)+SUM(HBP)+SUM(SF),0) AS lgOBP,
        (SUM(H)+SUM(`2B`)+2*SUM(`3B`)+3*SUM(HR)) / NULLIF(SUM(AB),0) AS lgSLG
    FROM season_bat GROUP BY yearID, lgID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG FROM teams WHERE lgID IN ('AL','NL') GROUP BY yearID
),
qualified_seasons AS (
    SELECT
        sb.playerID, sb.yearID, sb.PA,
        100 * (
            (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0) / la.lgOBP
          + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0) / la.lgSLG
          - 1
        ) AS ops_plus,
        ROW_NUMBER() OVER (PARTITION BY sb.playerID ORDER BY sb.yearID) AS season_num
    FROM season_bat sb
    JOIN league_avg la ON la.yearID = sb.yearID AND la.lgID = sb.lgID
    JOIN team_games tg ON tg.yearID = sb.yearID
    WHERE sb.PA >= 3.1 * tg.maxG AND la.lgOBP > 0 AND la.lgSLG > 0
),
five_yr_windows AS (
    SELECT
        qs.playerID,
        MIN(qs.yearID) AS window_start,
        MAX(qs.yearID) AS window_end,
        SUM(qs.PA) AS window_PA,
        ROUND(SUM(qs.ops_plus * qs.PA) / NULLIF(SUM(qs.PA),0), 1) AS peak_OPS_plus,
        qs.season_num - ROW_NUMBER() OVER (PARTITION BY qs.playerID ORDER BY qs.season_num) AS grp
    FROM qualified_seasons qs
    GROUP BY qs.playerID, grp
    HAVING COUNT(*) >= 5
),
/* For each player, find the best 5-consecutive-qualified-year window */
ranked_peaks AS (
    SELECT
        playerID,
        window_start,
        window_end,
        window_PA,
        peak_OPS_plus,
        ROW_NUMBER() OVER (PARTITION BY playerID ORDER BY peak_OPS_plus DESC) AS rn
    FROM five_yr_windows
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    rp.window_start,
    rp.window_end,
    rp.window_PA,
    rp.peak_OPS_plus,
    RANK() OVER (ORDER BY rp.peak_OPS_plus DESC) AS peak_rank
FROM ranked_peaks rp
JOIN people p ON p.playerID = rp.playerID
WHERE rp.rn = 1
ORDER BY peak_OPS_plus DESC;

-- ============================================================
-- View 38: v_era_career_arc_batting
-- Purpose: Decade-by-decade (age bucket) batting arcs showing
--          age curves.
-- Key formulas: Group by age bucket, compute AVG/OBP/SLG/OPS.
-- Qualifiers: career PA >= 3000, AL/NL
-- Notes: Age buckets: 20-24, 25-29, 30-34, 35-39, 40+.
--        Age = yearID - birthYear.
-- ============================================================
CREATE OR REPLACE VIEW v_era_career_arc_batting AS
WITH player_seasons AS (
    SELECT
        b.playerID,
        b.yearID,
        (b.yearID - p.birthYear) AS age,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b
    JOIN people p ON p.playerID = b.playerID
    WHERE b.lgID IN ('AL','NL') AND p.birthYear IS NOT NULL
    GROUP BY b.playerID, b.yearID, p.birthYear
),
career_pa AS (
    SELECT playerID, SUM(PA) AS total_PA
    FROM player_seasons
    GROUP BY playerID
    HAVING SUM(PA) >= 3000
)
SELECT
    ps.playerID,
    pe.nameFirst,
    pe.nameLast,
    CASE
        WHEN ps.age < 25 THEN '20-24'
        WHEN ps.age BETWEEN 25 AND 29 THEN '25-29'
        WHEN ps.age BETWEEN 30 AND 34 THEN '30-34'
        WHEN ps.age BETWEEN 35 AND 39 THEN '35-39'
        WHEN ps.age >= 40 THEN '40+'
    END AS age_bucket,
    SUM(ps.PA) AS bucket_PA,
    SUM(ps.AB) AS bucket_AB,
    SUM(ps.HR) AS bucket_HR,
    ROUND(SUM(ps.H) / NULLIF(SUM(ps.AB),0), 3) AS AVG,
    ROUND(
        (SUM(ps.H)+SUM(ps.BB)+SUM(ps.HBP))
        / NULLIF(SUM(ps.AB)+SUM(ps.BB)+SUM(ps.HBP)+SUM(ps.SF),0), 3
    ) AS OBP,
    ROUND(
        (SUM(ps.H)+SUM(ps.`2B`)+2*SUM(ps.`3B`)+3*SUM(ps.HR))
        / NULLIF(SUM(ps.AB),0), 3
    ) AS SLG,
    ROUND(
        (SUM(ps.H)+SUM(ps.BB)+SUM(ps.HBP))
        / NULLIF(SUM(ps.AB)+SUM(ps.BB)+SUM(ps.HBP)+SUM(ps.SF),0)
        + (SUM(ps.H)+SUM(ps.`2B`)+2*SUM(ps.`3B`)+3*SUM(ps.HR)) / NULLIF(SUM(ps.AB),0),
        3
    ) AS OPS
FROM player_seasons ps
JOIN career_pa cp ON cp.playerID = ps.playerID
JOIN people pe ON pe.playerID = ps.playerID
GROUP BY ps.playerID, pe.nameFirst, pe.nameLast, age_bucket
HAVING SUM(ps.AB) > 0
ORDER BY ps.playerID, age_bucket;

-- ============================================================
-- View 39: v_era_relative_dominance_batting
-- Purpose: How many standard deviations above league mean for
--          OPS, per season (z-score).
-- Key formulas: z = (playerOPS - lgMeanOPS) / lgStdDevOPS
-- Qualifiers: PA >= 400
-- Notes: Standard deviation computed across all qualified
--        hitters in that league-season.
--        Higher z-scores indicate greater relative dominance.
-- ============================================================
CREATE OR REPLACE VIEW v_era_relative_dominance_batting AS
WITH season_bat AS (
    SELECT
        b.playerID, b.yearID, b.lgID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA,
        /* OPS for this player-season */
        (SUM(b.H)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0)))
          / NULLIF(SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))+SUM(COALESCE(b.SF,0)),0)
        + (SUM(b.H)+SUM(b.`2B`)+2*SUM(b.`3B`)+3*SUM(b.HR))
          / NULLIF(SUM(b.AB),0) AS OPS
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
    HAVING SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) >= 400
       AND SUM(b.AB) > 0
),
league_stats AS (
    SELECT yearID, lgID,
        AVG(OPS) AS lg_mean_OPS,
        STDDEV_POP(OPS) AS lg_stddev_OPS
    FROM season_bat
    GROUP BY yearID, lgID
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sb.yearID,
    sb.lgID,
    sb.PA,
    ROUND(sb.OPS, 3) AS OPS,
    ROUND(ls.lg_mean_OPS, 3) AS lg_mean_OPS,
    ROUND(ls.lg_stddev_OPS, 3) AS lg_stddev_OPS,
    ROUND(
        (sb.OPS - ls.lg_mean_OPS) / NULLIF(ls.lg_stddev_OPS, 0), 2
    ) AS OPS_z_score,
    RANK() OVER (ORDER BY
        (sb.OPS - ls.lg_mean_OPS) / NULLIF(ls.lg_stddev_OPS, 0) DESC
    ) AS dominance_rank
FROM season_bat sb
JOIN league_stats ls ON ls.yearID = sb.yearID AND ls.lgID = sb.lgID
JOIN people p ON p.playerID = sb.playerID
WHERE ls.lg_stddev_OPS > 0
ORDER BY OPS_z_score DESC;
-- ============================================================
-- View 40: v_position_catcher_career
-- Purpose: Career catcher stats: batting + fielding (PB, CS%,
--          games caught), for catchers with >= 500 G as C.
-- Key formulas: CS% = CS / (CS + SB), Fld% = (PO+A)/(PO+A+E)
-- Qualifiers: >= 500 G at catcher position
-- Notes: Combines batting career totals with fielding stats
--        specifically at the catcher position.
-- ============================================================
CREATE OR REPLACE VIEW v_position_catcher_career AS
WITH catcher_fielding AS (
    SELECT
        f.playerID,
        SUM(f.G)                                           AS G_c,
        SUM(COALESCE(f.GS,0))                              AS GS_c,
        SUM(f.PO)                                          AS PO,
        SUM(f.A)                                           AS A,
        SUM(f.E)                                           AS E,
        SUM(COALESCE(f.DP,0))                              AS DP,
        SUM(COALESCE(f.PB,0))                              AS PB,
        SUM(COALESCE(f.SB,0))                              AS SB_allowed,
        SUM(COALESCE(f.CS,0))                              AS CS_caught,
        ROUND(
            (SUM(f.PO)+SUM(f.A)) / NULLIF(SUM(f.PO)+SUM(f.A)+SUM(f.E), 0), 4
        )                                                  AS Fld_pct,
        ROUND(
            SUM(COALESCE(f.CS,0))
            / NULLIF(SUM(COALESCE(f.CS,0)) + SUM(COALESCE(f.SB,0)), 0), 3
        )                                                  AS CS_pct
    FROM fielding f
    WHERE f.POS = 'C' AND f.lgID IN ('AL','NL')
    GROUP BY f.playerID
    HAVING SUM(f.G) >= 500
),
bat_career AS (
    SELECT
        b.playerID,
        SUM(b.G) AS G,
        SUM(b.AB) AS AB, SUM(b.H) AS H, SUM(b.HR) AS HR,
        SUM(COALESCE(b.RBI,0)) AS RBI,
        SUM(COALESCE(b.BB,0)) AS BB,
        SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`,
        ROUND(SUM(b.H)/NULLIF(SUM(b.AB),0), 3) AS AVG,
        ROUND(
            (SUM(b.H)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0)))
            /NULLIF(SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))+SUM(COALESCE(b.SF,0)),0), 3
        ) AS OBP,
        ROUND(
            (SUM(b.H)+SUM(b.`2B`)+2*SUM(b.`3B`)+3*SUM(b.HR))/NULLIF(SUM(b.AB),0), 3
        ) AS SLG
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    cf.G_c,
    cf.GS_c,
    bc.AB, bc.H, bc.HR, bc.RBI,
    bc.AVG, bc.OBP, bc.SLG,
    ROUND(bc.OBP + bc.SLG, 3) AS OPS,
    cf.PB,
    cf.SB_allowed,
    cf.CS_caught,
    cf.CS_pct,
    cf.Fld_pct,
    cf.E
FROM catcher_fielding cf
JOIN bat_career bc ON bc.playerID = cf.playerID
JOIN people p ON p.playerID = cf.playerID
ORDER BY cf.G_c DESC;

-- ============================================================
-- View 41: v_position_reliever_career
-- Purpose: Career reliever stats (GS < 10% of G): SV, holds
--          proxy, ERA, WHIP, K/9. >= 200 G.
-- Key formulas: ERA, WHIP, K/9. Reliever = GS < 10% of G.
--   Holds proxy: GF - SV (games finished minus saves is a
--   rough indicator of hold-like appearances).
-- Qualifiers: career G >= 200 and career GS < 0.10 * G
-- Notes: No true holds column in Lahman. GF - SV is an
--        approximation. Statcast/retrosheet data would be
--        needed for actual hold counts.
-- ============================================================
CREATE OR REPLACE VIEW v_position_reliever_career AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    SUM(pi.G)                                              AS G,
    SUM(pi.GS)                                             AS GS,
    SUM(COALESCE(pi.GF,0))                                 AS GF,
    SUM(COALESCE(pi.SV,0))                                 AS SV,
    /* Holds proxy: finished games minus saves */
    GREATEST(SUM(COALESCE(pi.GF,0)) - SUM(COALESCE(pi.SV,0)), 0) AS holds_proxy,
    SUM(pi.W)                                              AS W,
    SUM(pi.L)                                              AS L,
    ROUND(SUM(pi.IPouts)/3.0, 1)                           AS IP,
    ROUND(9.0*SUM(pi.ER)/NULLIF(SUM(pi.IPouts)/3.0, 0), 2) AS ERA,
    ROUND(
        (SUM(COALESCE(pi.BB,0))+SUM(pi.H))/NULLIF(SUM(pi.IPouts)/3.0, 0), 3
    )                                                      AS WHIP,
    ROUND(9.0*SUM(pi.SO)/NULLIF(SUM(pi.IPouts)/3.0, 0), 2) AS K_per_9,
    ROUND(9.0*SUM(COALESCE(pi.BB,0))/NULLIF(SUM(pi.IPouts)/3.0, 0), 2) AS BB_per_9,
    ROUND(SUM(pi.SO)/NULLIF(SUM(COALESCE(pi.BB,0)),0), 2) AS K_BB,
    /* FIP */
    ROUND(
        (13.0*SUM(pi.HR)+3.0*(SUM(COALESCE(pi.BB,0))+SUM(COALESCE(pi.HBP,0)))-2.0*SUM(pi.SO))
        /NULLIF(SUM(pi.IPouts)/3.0, 0) + 3.10, 2
    )                                                      AS FIP
FROM pitching pi
JOIN people p ON p.playerID = pi.playerID
WHERE pi.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING SUM(pi.G) >= 200
   AND SUM(pi.GS) < 0.10 * SUM(pi.G)
ORDER BY SV DESC;

-- ============================================================
-- View 42: v_position_dh_seasons
-- Purpose: DH-only seasons (G_dh > 100 from appearances),
--          with batting stats.
-- Key formulas: Standard batting rate stats.
-- Qualifiers: G_dh > 100 in a season
-- Notes: Uses appearances table for G_dh. DH role introduced
--        in AL in 1973, NL adopted universal DH in 2022.
-- ============================================================
CREATE OR REPLACE VIEW v_position_dh_seasons AS
WITH dh_seasons AS (
    SELECT a.playerID, a.yearID,
        SUM(COALESCE(a.G_dh,0)) AS G_dh,
        SUM(COALESCE(a.G_all,0)) AS G_all
    FROM appearances a
    WHERE a.lgID IN ('AL','NL')
    GROUP BY a.playerID, a.yearID
    HAVING SUM(COALESCE(a.G_dh,0)) > 100
),
season_bat AS (
    SELECT
        b.playerID, b.yearID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(COALESCE(b.RBI,0)) AS RBI,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SO,0)) AS SO
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    ds.yearID,
    ds.G_dh,
    ds.G_all,
    sb.AB, sb.H, sb.HR, sb.RBI,
    sb.BB, sb.SO,
    ROUND(sb.H/NULLIF(sb.AB,0), 3) AS AVG,
    ROUND(
        (sb.H+sb.BB+sb.HBP)/NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0), 3
    ) AS OBP,
    ROUND(
        (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR)/NULLIF(sb.AB,0), 3
    ) AS SLG,
    ROUND(
        (sb.H+sb.BB+sb.HBP)/NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0)
        + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR)/NULLIF(sb.AB,0), 3
    ) AS OPS
FROM dh_seasons ds
JOIN season_bat sb ON sb.playerID = ds.playerID AND sb.yearID = ds.yearID
JOIN people p ON p.playerID = ds.playerID
ORDER BY OPS DESC;

-- ============================================================
-- View 43: v_position_starter_vs_reliever_split
-- Purpose: Pitchers classified as SP vs RP each season, with
--          separate stat lines.
-- Key formulas: SP = GS >= 50% of G, RP = GS < 50% of G.
-- Qualifiers: IPouts >= 30 (10+ IP in a season)
-- Notes: Classification is per-season. A pitcher can be SP
--        one year and RP the next.
-- ============================================================
CREATE OR REPLACE VIEW v_position_starter_vs_reliever_split AS
WITH season_pitch AS (
    SELECT
        pi.playerID, pi.yearID,
        SUM(pi.G) AS G, SUM(pi.GS) AS GS,
        SUM(pi.IPouts) AS IPouts,
        SUM(pi.W) AS W, SUM(pi.L) AS L,
        SUM(COALESCE(pi.SV,0)) AS SV,
        SUM(pi.H) AS H, SUM(pi.ER) AS ER,
        SUM(COALESCE(pi.BB,0)) AS BB, SUM(pi.SO) AS SO,
        SUM(pi.HR) AS HR, SUM(COALESCE(pi.HBP,0)) AS HBP,
        SUM(COALESCE(pi.CG,0)) AS CG,
        SUM(COALESCE(pi.SHO,0)) AS SHO
    FROM pitching pi WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID
    HAVING SUM(pi.IPouts) >= 30
)
SELECT
    p.playerID, p.nameFirst, p.nameLast,
    sp.yearID,
    CASE WHEN sp.GS >= 0.5 * sp.G THEN 'SP' ELSE 'RP' END AS role,
    sp.G, sp.GS, sp.SV, sp.CG, sp.SHO,
    sp.W, sp.L,
    ROUND(sp.IPouts/3.0, 1) AS IP,
    ROUND(9.0*sp.ER/NULLIF(sp.IPouts/3.0,0), 2) AS ERA,
    ROUND((sp.BB+sp.H)/NULLIF(sp.IPouts/3.0,0), 3) AS WHIP,
    ROUND(9.0*sp.SO/NULLIF(sp.IPouts/3.0,0), 2) AS K_per_9,
    ROUND(9.0*sp.BB/NULLIF(sp.IPouts/3.0,0), 2) AS BB_per_9,
    ROUND(
        (13.0*sp.HR+3.0*(sp.BB+sp.HBP)-2.0*sp.SO)
        /NULLIF(sp.IPouts/3.0,0) + 3.10, 2
    ) AS FIP
FROM season_pitch sp
JOIN people p ON p.playerID = sp.playerID
ORDER BY sp.yearID DESC, role, ERA;

-- ============================================================
-- View 44: v_position_primary_by_career
-- Purpose: Each player's primary position based on most games
--          from the appearances table.
-- Key formulas: GREATEST of G_c, G_1b, G_2b, etc.
-- Qualifiers: AL/NL only
-- Notes: Includes G_p for pitchers. Returns one row per player
--        with the position where they played the most games.
-- ============================================================
CREATE OR REPLACE VIEW v_position_primary_by_career AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    SUM(COALESCE(a.G_all,0)) AS career_G,
    SUM(COALESCE(a.G_p,0)) AS G_p,
    SUM(COALESCE(a.G_c,0)) AS G_c,
    SUM(COALESCE(a.G_1b,0)) AS G_1b,
    SUM(COALESCE(a.G_2b,0)) AS G_2b,
    SUM(COALESCE(a.G_3b,0)) AS G_3b,
    SUM(COALESCE(a.G_ss,0)) AS G_ss,
    SUM(COALESCE(a.G_lf,0)) AS G_lf,
    SUM(COALESCE(a.G_cf,0)) AS G_cf,
    SUM(COALESCE(a.G_rf,0)) AS G_rf,
    SUM(COALESCE(a.G_of,0)) AS G_of,
    SUM(COALESCE(a.G_dh,0)) AS G_dh,
    CASE
        WHEN SUM(COALESCE(a.G_p,0))  >= GREATEST(SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'P'
        WHEN SUM(COALESCE(a.G_c,0))  >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'C'
        WHEN SUM(COALESCE(a.G_1b,0)) >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN '1B'
        WHEN SUM(COALESCE(a.G_2b,0)) >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN '2B'
        WHEN SUM(COALESCE(a.G_3b,0)) >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN '3B'
        WHEN SUM(COALESCE(a.G_ss,0)) >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'SS'
        WHEN SUM(COALESCE(a.G_cf,0)) >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'CF'
        WHEN SUM(COALESCE(a.G_rf,0)) >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'RF'
        WHEN SUM(COALESCE(a.G_lf,0)) >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0)),SUM(COALESCE(a.G_dh,0))) THEN 'LF'
        WHEN SUM(COALESCE(a.G_dh,0)) >= GREATEST(SUM(COALESCE(a.G_p,0)),SUM(COALESCE(a.G_c,0)),SUM(COALESCE(a.G_1b,0)),SUM(COALESCE(a.G_2b,0)),SUM(COALESCE(a.G_3b,0)),SUM(COALESCE(a.G_ss,0)),SUM(COALESCE(a.G_lf,0)),SUM(COALESCE(a.G_cf,0)),SUM(COALESCE(a.G_rf,0))) THEN 'DH'
        ELSE 'OF'
    END AS primary_pos
FROM appearances a
JOIN people p ON p.playerID = a.playerID
WHERE a.lgID IN ('AL','NL')
GROUP BY p.playerID, p.nameFirst, p.nameLast
ORDER BY career_G DESC;

-- ============================================================
-- View 45: v_position_offensive_by_position
-- Purpose: Average offensive output (OPS, wOBA) by position
--          per era, to see positional value over time.
-- Key formulas: OPS, wOBA aggregated by position and era.
-- Qualifiers: Uses appearances-based primary position per
--             player-season. PA >= 200 per season.
-- Notes: Useful for understanding how offensive expectations
--        differ by position and how that has evolved.
--        Would benefit from Statcast data for wRC+.
-- ============================================================
CREATE OR REPLACE VIEW v_position_offensive_by_position AS
WITH player_season_pos AS (
    SELECT
        a.playerID, a.yearID,
        CASE
            WHEN COALESCE(a.G_c,0) >= GREATEST(COALESCE(a.G_1b,0),COALESCE(a.G_2b,0),COALESCE(a.G_3b,0),COALESCE(a.G_ss,0),COALESCE(a.G_lf,0),COALESCE(a.G_cf,0),COALESCE(a.G_rf,0),COALESCE(a.G_dh,0)) THEN 'C'
            WHEN COALESCE(a.G_1b,0) >= GREATEST(COALESCE(a.G_c,0),COALESCE(a.G_2b,0),COALESCE(a.G_3b,0),COALESCE(a.G_ss,0),COALESCE(a.G_lf,0),COALESCE(a.G_cf,0),COALESCE(a.G_rf,0),COALESCE(a.G_dh,0)) THEN '1B'
            WHEN COALESCE(a.G_2b,0) >= GREATEST(COALESCE(a.G_c,0),COALESCE(a.G_1b,0),COALESCE(a.G_3b,0),COALESCE(a.G_ss,0),COALESCE(a.G_lf,0),COALESCE(a.G_cf,0),COALESCE(a.G_rf,0),COALESCE(a.G_dh,0)) THEN '2B'
            WHEN COALESCE(a.G_3b,0) >= GREATEST(COALESCE(a.G_c,0),COALESCE(a.G_1b,0),COALESCE(a.G_2b,0),COALESCE(a.G_ss,0),COALESCE(a.G_lf,0),COALESCE(a.G_cf,0),COALESCE(a.G_rf,0),COALESCE(a.G_dh,0)) THEN '3B'
            WHEN COALESCE(a.G_ss,0) >= GREATEST(COALESCE(a.G_c,0),COALESCE(a.G_1b,0),COALESCE(a.G_2b,0),COALESCE(a.G_3b,0),COALESCE(a.G_lf,0),COALESCE(a.G_cf,0),COALESCE(a.G_rf,0),COALESCE(a.G_dh,0)) THEN 'SS'
            WHEN COALESCE(a.G_lf,0) >= GREATEST(COALESCE(a.G_c,0),COALESCE(a.G_1b,0),COALESCE(a.G_2b,0),COALESCE(a.G_3b,0),COALESCE(a.G_ss,0),COALESCE(a.G_cf,0),COALESCE(a.G_rf,0),COALESCE(a.G_dh,0)) THEN 'LF'
            WHEN COALESCE(a.G_cf,0) >= GREATEST(COALESCE(a.G_c,0),COALESCE(a.G_1b,0),COALESCE(a.G_2b,0),COALESCE(a.G_3b,0),COALESCE(a.G_ss,0),COALESCE(a.G_lf,0),COALESCE(a.G_rf,0),COALESCE(a.G_dh,0)) THEN 'CF'
            WHEN COALESCE(a.G_rf,0) >= GREATEST(COALESCE(a.G_c,0),COALESCE(a.G_1b,0),COALESCE(a.G_2b,0),COALESCE(a.G_3b,0),COALESCE(a.G_ss,0),COALESCE(a.G_lf,0),COALESCE(a.G_cf,0),COALESCE(a.G_dh,0)) THEN 'RF'
            WHEN COALESCE(a.G_dh,0) >= GREATEST(COALESCE(a.G_c,0),COALESCE(a.G_1b,0),COALESCE(a.G_2b,0),COALESCE(a.G_3b,0),COALESCE(a.G_ss,0),COALESCE(a.G_lf,0),COALESCE(a.G_cf,0),COALESCE(a.G_rf,0)) THEN 'DH'
            ELSE 'OF'
        END AS pos
    FROM appearances a
    WHERE a.lgID IN ('AL','NL')
      AND (COALESCE(a.G_c,0)+COALESCE(a.G_1b,0)+COALESCE(a.G_2b,0)+COALESCE(a.G_3b,0)
           +COALESCE(a.G_ss,0)+COALESCE(a.G_lf,0)+COALESCE(a.G_cf,0)+COALESCE(a.G_rf,0)
           +COALESCE(a.G_dh,0)) > 0
),
season_bat AS (
    SELECT
        b.playerID, b.yearID,
        SUM(b.AB) AS AB, SUM(b.H) AS H,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`, SUM(b.HR) AS HR,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.IBB,0)) AS IBB,
        SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID
    HAVING SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) >= 200
       AND SUM(b.AB) > 0
)
SELECT
    psp.pos AS position,
    CASE
        WHEN sb.yearID BETWEEN 1901 AND 1919 THEN 'Dead Ball'
        WHEN sb.yearID BETWEEN 1920 AND 1941 THEN 'Live Ball'
        WHEN sb.yearID BETWEEN 1942 AND 1960 THEN 'Integration'
        WHEN sb.yearID BETWEEN 1961 AND 1976 THEN 'Expansion'
        WHEN sb.yearID BETWEEN 1977 AND 1993 THEN 'Free Agency'
        WHEN sb.yearID BETWEEN 1994 AND 2005 THEN 'Steroid'
        WHEN sb.yearID BETWEEN 2006 AND 2015 THEN 'Post-Steroid'
        WHEN sb.yearID >= 2016                THEN 'Statcast/Analytics'
        ELSE 'Pre-Modern'
    END AS era_name,
    COUNT(*) AS player_seasons,
    ROUND(AVG(sb.H / NULLIF(sb.AB,0)), 3) AS avg_AVG,
    ROUND(AVG(
        (sb.H+sb.BB+sb.HBP) / NULLIF(sb.AB+sb.BB+sb.HBP+sb.SF,0)
        + (sb.H+sb.`2B`+2*sb.`3B`+3*sb.HR) / NULLIF(sb.AB,0)
    ), 3) AS avg_OPS,
    ROUND(AVG(
        (0.69*(sb.BB-sb.IBB)+0.72*sb.HBP
         +0.87*(sb.H-sb.`2B`-sb.`3B`-sb.HR)
         +1.22*sb.`2B`+1.56*sb.`3B`+1.95*sb.HR)
        / NULLIF(sb.AB+sb.BB-sb.IBB+sb.SF+sb.HBP,0)
    ), 3) AS avg_wOBA,
    ROUND(AVG(sb.HR * 1.0), 1) AS avg_HR
FROM season_bat sb
JOIN player_season_pos psp ON psp.playerID = sb.playerID AND psp.yearID = sb.yearID
WHERE sb.yearID >= 1901
GROUP BY psp.pos, era_name
ORDER BY psp.pos, MIN(sb.yearID);
-- ============================================================
-- View 46: v_postseason_batting_career
-- Purpose: Career postseason batting totals: G, AB, H, HR,
--          RBI, AVG, OBP, SLG, OPS. >= 50 postseason AB.
-- Key formulas: Standard rate stats from battingpost.
-- Qualifiers: career postseason AB >= 50
-- Notes: Aggregates all rounds (WS, ALCS, NLCS, ALDS, etc.).
-- ============================================================
CREATE OR REPLACE VIEW v_postseason_batting_career AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    SUM(bp.G)                                              AS G,
    SUM(bp.AB)                                             AS AB,
    SUM(COALESCE(bp.R,0))                                  AS R,
    SUM(bp.H)                                              AS H,
    SUM(bp.`2B`)                                           AS `2B`,
    SUM(bp.`3B`)                                           AS `3B`,
    SUM(bp.HR)                                             AS HR,
    SUM(COALESCE(bp.RBI,0))                                AS RBI,
    SUM(COALESCE(bp.SB,0))                                 AS SB,
    SUM(COALESCE(bp.BB,0))                                 AS BB,
    SUM(COALESCE(bp.SO,0))                                 AS SO,
    SUM(bp.AB) + SUM(COALESCE(bp.BB,0))
               + SUM(COALESCE(bp.HBP,0))
               + SUM(COALESCE(bp.SF,0))
               + SUM(COALESCE(bp.SH,0))                   AS PA,
    ROUND(SUM(bp.H) / NULLIF(SUM(bp.AB),0), 3)            AS AVG,
    ROUND(
        (SUM(bp.H)+SUM(COALESCE(bp.BB,0))+SUM(COALESCE(bp.HBP,0)))
        / NULLIF(SUM(bp.AB)+SUM(COALESCE(bp.BB,0))
                 +SUM(COALESCE(bp.HBP,0))+SUM(COALESCE(bp.SF,0)),0), 3
    )                                                      AS OBP,
    ROUND(
        (SUM(bp.H)+SUM(bp.`2B`)+2*SUM(bp.`3B`)+3*SUM(bp.HR))
        / NULLIF(SUM(bp.AB),0), 3
    )                                                      AS SLG,
    ROUND(
        (SUM(bp.H)+SUM(COALESCE(bp.BB,0))+SUM(COALESCE(bp.HBP,0)))
        / NULLIF(SUM(bp.AB)+SUM(COALESCE(bp.BB,0))
                 +SUM(COALESCE(bp.HBP,0))+SUM(COALESCE(bp.SF,0)),0)
        + (SUM(bp.H)+SUM(bp.`2B`)+2*SUM(bp.`3B`)+3*SUM(bp.HR))
          / NULLIF(SUM(bp.AB),0), 3
    )                                                      AS OPS
FROM battingpost bp
JOIN people p ON p.playerID = bp.playerID
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING SUM(bp.AB) >= 50
ORDER BY OPS DESC;

-- ============================================================
-- View 47: v_postseason_pitching_career
-- Purpose: Career postseason pitching: W, L, ERA, IP, SO,
--          WHIP. >= 30 postseason IPouts (10 IP).
-- Key formulas: ERA = 9*ER/(IPouts/3), WHIP = (BB+H)/(IPouts/3)
-- Qualifiers: career postseason IPouts >= 30
-- Notes: Aggregates all postseason rounds.
-- ============================================================
CREATE OR REPLACE VIEW v_postseason_pitching_career AS
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    SUM(pp.G)                                              AS G,
    SUM(pp.GS)                                             AS GS,
    SUM(pp.W)                                              AS W,
    SUM(pp.L)                                              AS L,
    SUM(COALESCE(pp.SV,0))                                 AS SV,
    SUM(COALESCE(pp.CG,0))                                 AS CG,
    SUM(COALESCE(pp.SHO,0))                                AS SHO,
    ROUND(SUM(pp.IPouts)/3.0, 1)                           AS IP,
    SUM(pp.H)                                              AS H,
    SUM(pp.ER)                                             AS ER,
    SUM(pp.HR)                                             AS HR,
    SUM(COALESCE(pp.BB,0))                                 AS BB,
    SUM(pp.SO)                                             AS SO,
    ROUND(9.0*SUM(pp.ER)/NULLIF(SUM(pp.IPouts)/3.0,0), 2) AS ERA,
    ROUND(
        (SUM(COALESCE(pp.BB,0))+SUM(pp.H))
        /NULLIF(SUM(pp.IPouts)/3.0,0), 3
    )                                                      AS WHIP,
    ROUND(9.0*SUM(pp.SO)/NULLIF(SUM(pp.IPouts)/3.0,0), 2) AS K_per_9
FROM pitchingpost pp
JOIN people p ON p.playerID = pp.playerID
GROUP BY p.playerID, p.nameFirst, p.nameLast
HAVING SUM(pp.IPouts) >= 30
ORDER BY ERA ASC;

-- ============================================================
-- View 48: v_postseason_clutch_performers
-- Purpose: Combined postseason batting + pitching excellence,
--          top performers ranked by composite postseason score.
-- Key formulas: Composite = batting_score + pitching_score
--   batting_score = (OPS - 0.700) * PA / 50 + HR * 2
--   pitching_score = (4.00 - ERA) * IP / 20 + W * 3 + SV * 2
-- Qualifiers: postseason AB >= 30 OR postseason IPouts >= 20
-- Notes: This composite is an arbitrary but balanced metric.
--        Positive batting_score means above league-average
--        postseason hitting. Positive pitching_score means
--        above-average postseason pitching.
-- ============================================================
CREATE OR REPLACE VIEW v_postseason_clutch_performers AS
WITH ps_batting AS (
    SELECT
        bp.playerID,
        SUM(bp.G) AS G,
        SUM(bp.AB) AS AB,
        SUM(bp.H) AS H, SUM(bp.HR) AS HR,
        SUM(COALESCE(bp.RBI,0)) AS RBI,
        SUM(COALESCE(bp.BB,0)) AS BB,
        SUM(COALESCE(bp.HBP,0)) AS HBP,
        SUM(COALESCE(bp.SF,0)) AS SF,
        SUM(COALESCE(bp.SH,0)) AS SH,
        SUM(bp.`2B`) AS `2B`, SUM(bp.`3B`) AS `3B`,
        SUM(bp.AB)+SUM(COALESCE(bp.BB,0))+SUM(COALESCE(bp.HBP,0))
            +SUM(COALESCE(bp.SF,0))+SUM(COALESCE(bp.SH,0)) AS PA
    FROM battingpost bp
    GROUP BY bp.playerID
    HAVING SUM(bp.AB) >= 30
),
ps_pitching AS (
    SELECT
        pp.playerID,
        SUM(pp.G) AS G,
        SUM(pp.W) AS W, SUM(pp.L) AS L,
        SUM(COALESCE(pp.SV,0)) AS SV,
        SUM(pp.IPouts) AS IPouts,
        SUM(pp.ER) AS ER, SUM(pp.SO) AS SO
    FROM pitchingpost pp
    GROUP BY pp.playerID
    HAVING SUM(pp.IPouts) >= 20
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    /* Batting postseason stats */
    COALESCE(pb.G, 0) AS bat_G,
    COALESCE(pb.AB, 0) AS bat_AB,
    COALESCE(pb.PA, 0) AS bat_PA,
    COALESCE(pb.HR, 0) AS bat_HR,
    COALESCE(pb.RBI, 0) AS bat_RBI,
    CASE WHEN pb.AB > 0 THEN
        ROUND(pb.H / NULLIF(pb.AB, 0), 3)
    END AS bat_AVG,
    CASE WHEN pb.AB > 0 THEN
        ROUND(
            (pb.H+pb.BB+pb.HBP)/NULLIF(pb.AB+pb.BB+pb.HBP+pb.SF,0)
            + (pb.H+pb.`2B`+2*pb.`3B`+3*pb.HR)/NULLIF(pb.AB,0), 3
        )
    END AS bat_OPS,
    /* Pitching postseason stats */
    COALESCE(pp.G, 0) AS pitch_G,
    COALESCE(pp.W, 0) AS pitch_W,
    COALESCE(pp.L, 0) AS pitch_L,
    COALESCE(pp.SV, 0) AS pitch_SV,
    CASE WHEN pp.IPouts > 0 THEN ROUND(pp.IPouts/3.0, 1) END AS pitch_IP,
    CASE WHEN pp.IPouts > 0 THEN
        ROUND(9.0*pp.ER/NULLIF(pp.IPouts/3.0,0), 2)
    END AS pitch_ERA,
    /* Composite score */
    ROUND(
        COALESCE(
            CASE WHEN pb.AB > 0 THEN
                ((pb.H+pb.BB+pb.HBP)/NULLIF(pb.AB+pb.BB+pb.HBP+pb.SF,0)
                 + (pb.H+pb.`2B`+2*pb.`3B`+3*pb.HR)/NULLIF(pb.AB,0)
                 - 0.700) * pb.PA / 50.0
                + pb.HR * 2.0
            END, 0)
        + COALESCE(
            CASE WHEN pp.IPouts > 0 THEN
                (4.00 - 9.0*pp.ER/NULLIF(pp.IPouts/3.0,0)) * (pp.IPouts/3.0) / 20.0
                + pp.W * 3.0
                + pp.SV * 2.0
            END, 0),
        1
    ) AS clutch_composite,
    RANK() OVER (ORDER BY
        COALESCE(
            CASE WHEN pb.AB > 0 THEN
                ((pb.H+pb.BB+pb.HBP)/NULLIF(pb.AB+pb.BB+pb.HBP+pb.SF,0)
                 + (pb.H+pb.`2B`+2*pb.`3B`+3*pb.HR)/NULLIF(pb.AB,0)
                 - 0.700) * pb.PA / 50.0
                + pb.HR * 2.0
            END, 0)
        + COALESCE(
            CASE WHEN pp.IPouts > 0 THEN
                (4.00 - 9.0*pp.ER/NULLIF(pp.IPouts/3.0,0)) * (pp.IPouts/3.0) / 20.0
                + pp.W * 3.0
                + pp.SV * 2.0
            END, 0)
        DESC
    ) AS clutch_rank
FROM people p
LEFT JOIN ps_batting pb ON pb.playerID = p.playerID
LEFT JOIN ps_pitching pp ON pp.playerID = p.playerID
WHERE pb.playerID IS NOT NULL OR pp.playerID IS NOT NULL
ORDER BY clutch_composite DESC;
-- ============================================================
-- View 49: v_milestone_50hr_seasons
-- Purpose: All 50+ HR seasons with player info and era context.
-- Key formulas: SUM(HR) per player per year.
-- Qualifiers: HR >= 50 in a season
-- Notes: One of baseball's most exclusive clubs. Includes era
--        label and league context for interpretation.
-- ============================================================
CREATE OR REPLACE VIEW v_milestone_50hr_seasons AS
WITH season_hr AS (
    SELECT
        b.playerID, b.yearID,
        GROUP_CONCAT(DISTINCT b.teamID ORDER BY b.stint) AS teams,
        SUM(b.HR) AS HR,
        SUM(b.AB) AS AB,
        SUM(b.H) AS H,
        SUM(COALESCE(b.RBI,0)) AS RBI,
        SUM(COALESCE(b.BB,0)) AS BB,
        SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`
    FROM batting b
    WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID
    HAVING SUM(b.HR) >= 50
)
SELECT
    p.playerID,
    p.nameFirst,
    p.nameLast,
    sh.yearID,
    sh.teams,
    sh.HR,
    sh.AB,
    sh.RBI,
    ROUND(sh.H / NULLIF(sh.AB,0), 3) AS AVG,
    ROUND(
        (sh.H+sh.BB+sh.HBP) / NULLIF(sh.AB+sh.BB+sh.HBP+sh.SF,0)
        + (sh.H+sh.`2B`+2*sh.`3B`+3*sh.HR) / NULLIF(sh.AB,0), 3
    ) AS OPS,
    (sh.yearID - p.birthYear) AS age,
    CASE
        WHEN sh.yearID BETWEEN 1901 AND 1919 THEN 'Dead Ball'
        WHEN sh.yearID BETWEEN 1920 AND 1941 THEN 'Live Ball'
        WHEN sh.yearID BETWEEN 1942 AND 1960 THEN 'Integration'
        WHEN sh.yearID BETWEEN 1961 AND 1976 THEN 'Expansion'
        WHEN sh.yearID BETWEEN 1977 AND 1993 THEN 'Free Agency'
        WHEN sh.yearID BETWEEN 1994 AND 2005 THEN 'Steroid'
        WHEN sh.yearID BETWEEN 2006 AND 2015 THEN 'Post-Steroid'
        WHEN sh.yearID >= 2016                THEN 'Statcast/Analytics'
        ELSE 'Pre-Modern'
    END AS era_name,
    ROW_NUMBER() OVER (ORDER BY sh.HR DESC, sh.yearID) AS hr_rank
FROM season_hr sh
JOIN people p ON p.playerID = sh.playerID
ORDER BY sh.HR DESC, sh.yearID;

-- ============================================================
-- View 50: v_milestone_historic_achievements
-- Purpose: Identifies historic achievements: 40-40 club,
--          .400 hitters, 300-win pitchers, 3000-hit club,
--          500-HR club, sub-1.50 ERA qualified seasons,
--          30-win seasons, triple crowns (batting).
-- Key formulas: Various milestone thresholds.
-- Qualifiers: Each milestone has its own threshold.
-- Notes: This is a UNION ALL of multiple milestone queries.
--        Triple Crown: led league in AVG, HR, and RBI.
--        For batting qualifiers, uses PA >= 3.1 * teamG.
-- ============================================================
CREATE OR REPLACE VIEW v_milestone_historic_achievements AS
/* 40-40 club: 40+ HR and 40+ SB in same season */
WITH season_bat AS (
    SELECT b.playerID, b.yearID,
        SUM(b.AB) AS AB, SUM(b.H) AS H, SUM(b.HR) AS HR,
        SUM(COALESCE(b.RBI,0)) AS RBI,
        SUM(COALESCE(b.SB,0)) AS SB,
        SUM(COALESCE(b.BB,0)) AS BB, SUM(COALESCE(b.HBP,0)) AS HBP,
        SUM(COALESCE(b.SF,0)) AS SF, SUM(COALESCE(b.SH,0)) AS SH,
        SUM(b.`2B`) AS `2B`, SUM(b.`3B`) AS `3B`,
        SUM(b.AB)+SUM(COALESCE(b.BB,0))+SUM(COALESCE(b.HBP,0))
            +SUM(COALESCE(b.SF,0))+SUM(COALESCE(b.SH,0)) AS PA,
        b.lgID
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID, b.yearID, b.lgID
),
team_games AS (
    SELECT yearID, MAX(G) AS maxG FROM teams WHERE lgID IN ('AL','NL') GROUP BY yearID
),
/* 40-40 */
forty_forty AS (
    SELECT playerID, yearID, HR, SB,
        '40-40 Club' AS milestone,
        CONCAT(HR, ' HR / ', SB, ' SB') AS detail
    FROM season_bat WHERE HR >= 40 AND SB >= 40
),
/* .400 hitters (qualified) */
four_hundred_avg AS (
    SELECT sb.playerID, sb.yearID, sb.H, sb.AB,
        '.400 Batting Average' AS milestone,
        CONCAT(ROUND(sb.H/sb.AB, 3), ' AVG (', sb.H, '-for-', sb.AB, ')') AS detail
    FROM season_bat sb
    JOIN team_games tg ON tg.yearID = sb.yearID
    WHERE sb.PA >= 3.1 * tg.maxG
      AND sb.AB > 0
      AND (sb.H / sb.AB) >= 0.400
),
/* 3000-hit club (career) */
three_thousand_hits AS (
    SELECT b.playerID, MAX(b.yearID) AS yearID,
        SUM(b.H) AS career_H,
        '3000-Hit Club' AS milestone,
        CONCAT(SUM(b.H), ' career hits') AS detail
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID
    HAVING SUM(b.H) >= 3000
),
/* 500-HR club (career) */
five_hundred_hr AS (
    SELECT b.playerID, MAX(b.yearID) AS yearID,
        SUM(b.HR) AS career_HR,
        '500-HR Club' AS milestone,
        CONCAT(SUM(b.HR), ' career HR') AS detail
    FROM batting b WHERE b.lgID IN ('AL','NL')
    GROUP BY b.playerID
    HAVING SUM(b.HR) >= 500
),
/* 300-win pitchers (career) */
three_hundred_wins AS (
    SELECT pi.playerID, MAX(pi.yearID) AS yearID,
        SUM(pi.W) AS career_W,
        '300-Win Club' AS milestone,
        CONCAT(SUM(pi.W), ' career wins') AS detail
    FROM pitching pi WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID
    HAVING SUM(pi.W) >= 300
),
/* Sub-1.50 ERA qualified seasons */
sub_150_era AS (
    SELECT sp.playerID, sp.yearID,
        sp.IPouts, sp.ER,
        'Sub-1.50 ERA Season' AS milestone,
        CONCAT(ROUND(9.0*sp.ER/(sp.IPouts/3.0), 2), ' ERA in ',
               ROUND(sp.IPouts/3.0, 1), ' IP') AS detail
    FROM (
        SELECT playerID, yearID,
            SUM(IPouts) AS IPouts, SUM(ER) AS ER
        FROM pitching WHERE lgID IN ('AL','NL')
        GROUP BY playerID, yearID
    ) sp
    JOIN team_games tg ON tg.yearID = sp.yearID
    WHERE sp.IPouts >= 3 * tg.maxG
      AND sp.IPouts > 0
      AND sp.ER >= 0
      AND (9.0 * sp.ER / (sp.IPouts/3.0)) < 1.50
),
/* 30-win seasons */
thirty_win_seasons AS (
    SELECT pi.playerID, pi.yearID,
        SUM(pi.W) AS W,
        '30-Win Season' AS milestone,
        CONCAT(SUM(pi.W), ' wins') AS detail
    FROM pitching pi WHERE pi.lgID IN ('AL','NL')
    GROUP BY pi.playerID, pi.yearID
    HAVING SUM(pi.W) >= 30
),
/* Batting Triple Crown: led league in AVG, HR, and RBI (qualified) */
lg_leaders AS (
    SELECT sb.yearID, sb.lgID, sb.playerID,
        sb.H / NULLIF(sb.AB,0) AS AVG,
        sb.HR, sb.RBI,
        RANK() OVER (PARTITION BY sb.yearID, sb.lgID ORDER BY sb.H/NULLIF(sb.AB,0) DESC) AS avg_rank,
        RANK() OVER (PARTITION BY sb.yearID, sb.lgID ORDER BY sb.HR DESC) AS hr_rank,
        RANK() OVER (PARTITION BY sb.yearID, sb.lgID ORDER BY sb.RBI DESC) AS rbi_rank
    FROM season_bat sb
    JOIN team_games tg ON tg.yearID = sb.yearID
    WHERE sb.PA >= 3.1 * tg.maxG AND sb.AB > 0
),
triple_crown AS (
    SELECT playerID, yearID, HR, RBI,
        'Batting Triple Crown' AS milestone,
        CONCAT(ROUND(AVG, 3), ' AVG / ', HR, ' HR / ', RBI, ' RBI') AS detail
    FROM lg_leaders
    WHERE avg_rank = 1 AND hr_rank = 1 AND rbi_rank = 1
)
/* Union all milestones */
SELECT p.playerID, p.nameFirst, p.nameLast, m.yearID, m.milestone, m.detail
FROM (
    SELECT playerID, yearID, milestone, detail FROM forty_forty
    UNION ALL
    SELECT playerID, yearID, milestone, detail FROM four_hundred_avg
    UNION ALL
    SELECT playerID, yearID, milestone, detail FROM three_thousand_hits
    UNION ALL
    SELECT playerID, yearID, milestone, detail FROM five_hundred_hr
    UNION ALL
    SELECT playerID, yearID, milestone, detail FROM three_hundred_wins
    UNION ALL
    SELECT playerID, yearID, milestone, detail FROM sub_150_era
    UNION ALL
    SELECT playerID, yearID, milestone, detail FROM thirty_win_seasons
    UNION ALL
    SELECT playerID, yearID, milestone, detail FROM triple_crown
) m
JOIN people p ON p.playerID = m.playerID
ORDER BY m.milestone, m.yearID, p.nameLast;
