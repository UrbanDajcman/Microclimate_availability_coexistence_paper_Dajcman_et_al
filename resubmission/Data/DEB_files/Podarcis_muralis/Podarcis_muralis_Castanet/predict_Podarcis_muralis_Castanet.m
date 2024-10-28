function [prdData, info] = predict_Podarcis_muralis_Castanet(par, data, auxData)
  
  % unpack par, data, auxData
  cPar = parscomp_st(par); vars_pull(par); 
  vars_pull(cPar);  vars_pull(data);  vars_pull(auxData);
  
  % compute temperature correction factors
  pars_T = T_A;
  TC_ab = tempcorr(temp.ab, T_ref, T_A);
  TC = tempcorr(temp.am, T_ref, T_A); kT_M = TC * k_M;
  TC_LN = tempcorr(temp.LN, T_ref, pars_T);
  TC_Tab = tempcorr(C2K(Tab(:,1)), T_ref, pars_T);
   %TC_Tab = tempcorr(C2K(Tab(:,1)), T_ref, pars_T);
  TC_WO_23 = tempcorr(temp.WwJO_23, T_ref, T_A);
  TC_WO_28 = tempcorr(temp.WwJO_28, T_ref, T_A);
  TC_WO_33 = tempcorr(temp.WwJO_33, T_ref, T_A);
  
% zero-variate data

  % life cycle
  pars_tp = [g k l_T v_Hb v_Hp];
  [t_p, t_b, l_p, l_b, info] = get_tp(pars_tp, f);
  
  % initial
  pars_UE0 = [V_Hb; g; k_J; k_M; v]; % compose parameter vector
  U_E0 = initial_scaled_reserve(f, pars_UE0); % d.cm^2, initial scaled reserve
  Ww_0 = U_E0 * p_Am * w_E/ mu_E/ d_E; % g, egg wet weight

  % birth
  L_b = L_m * l_b;                  % cm, structural length at birth at f
  Lw_b = L_b/ del_M;                % cm, SVL at birth
  aT_b = t_b/ k_M/ TC_ab;           % d, age at birth at f and T
  Ww_b = L_b^3 * (1 + f * w);       % g, wet weight at birth at f

  % puberty 
  L_p = L_m * l_p;                  % cm, struc length at puberty
  Lw_p = L_p/ del_M;                % cm, SVL at puberty
  tT_p = (t_p - t_b)/ k_M/ TC;   % d, age at puberty at f and T

  % ultimate
  l_i = f - l_T;                    % -, scaled ultimate length
  L_i = L_m * l_i;                  % cm, ultimate structural length at f
  Lw_i = L_i/ del_M;                % cm, ultimate SVL
  Ww_i = L_i^3 * (1 + f * w);       % g, ultimate wet weight at f

  % reproduction
  pars_R = [kap; kap_R; g; k_J; k_M; L_T; v; U_Hb; U_Hp]; % compose parameter vector at T
  RT_i = TC * reprod_rate(L_i, f, pars_R);                % #/d, ultimate reproduction rate at T

  % life span
  pars_tm = [g; l_T; h_a/ k_M^2; s_G];  % compose parameter vector at T_ref
  t_m = get_tm_s(pars_tm, f, l_b);      % -, scaled mean life span at T_ref
  aT_m = t_m/ k_M/ TC;                  % d, mean life span at T
  
  % males
  p_Am_m = z_m * p_M/ kap; % J/d.cm^2, {p_Am} spec assimilation flux
  E_m_m = p_Am_m/ v; % J/cm^3, reserve capacity [E_m]
  g_m = E_G/ (kap* E_m_m); % -, energy investment ratio
  m_Em_m = y_E_V * E_m_m/ E_G; % mol/mol, reserve capacity
  ome_m = m_Em_m * w_E/ w_V; % -, contribution of reserve to weight
  L_mm = v/ k_M/ g_m; L_im = f * L_mm; Lw_im = L_im/ del_M_m; % cm, max ultimate length
  pars_tpm = [g_m k l_T v_Hb v_Hpm]; % pars for males
  [tau_pm, tau_bm, l_pm, l_bm] = get_tp(pars_tpm, f, [t_b f*z/z_m l_b*z/z_m]); % - , scaled time, length
  L_bm = L_mm * l_bm; L_pm = L_mm * l_pm; Lw_pm = L_pm/ del_M_m;  % cm, lengths
  Ww_im = L_im^3 * (1 + f * ome_m); % g, ultimate wet weight
  w_m = m_Em_m * w_E/ w_V;             % -, contribution of reserve to weight

  % pack to output
  prdData.ab = aT_b;
  prdData.tp = tT_p;
  prdData.am = aT_m;
  prdData.Lb = Lw_b;
  prdData.Lp = Lw_p;
  prdData.Lpm = Lw_pm;
  prdData.Li = Lw_i;
  prdData.Lim = Lw_im;
  prdData.Ww0 = Ww_0;
  prdData.Wwb = Ww_b;
  prdData.Wwi = Ww_i;
  prdData.Wwim = Ww_im;
  prdData.Ri = RT_i;
  
  % univar data
  
  % time-length
  % female
  [tvel, t_p, t_b, l_p, l_b] = get_tp(pars_tp, f, [], tL_f(:,1)*kT_M);
  ELw_f = L_m * tvel(:,4)/ del_M;   % cm, length 
  % male
  tvel = get_tp(pars_tpm, f, [t_b, f*z/z_m, l_b*z/z_m], tL_m(:,1)*kT_M);
  ELw_m = L_mm * tvel(:,4)/ del_M;   % cm, length 
  
  % length-weight data
  EWw_f = (LW_f(:,1) * del_M).^3 * (1 + w * f);  % g, wet weight females
  EWw_m = (LW_m(:,1) * del_M_m).^3 * (1 + w_m * f);% g, wet weight males
 
  % length - yearly fecundity
  pars_R = [kap; kap_R; g; k_J; k_M; L_T; v; U_Hb; U_Hp]; % compose parameter vector
  LN = TC_LN * 365 * reprod_rate(LN(:,1) * del_M, f, pars_R); % #, yearly fecundity
  
  % temperature-development
  aT_b2 = t_b/ k_M ./ TC_Tab;           % d, age at birth at f and T
  
   % length-respiration
  O2M = (- n_M\n_O)'; % -, matrix that converts organic to mineral fluxes  O2M is prepared for post-multiplication eq. 4.35
  p_ref = p_Am * L_m^2;               % J/d, max assimilation power at max size
  pars_power = [kap; kap_R; g; k_J; k_M; L_T; v; U_Hb; U_Hp];

  % temp = 23 C
  % L = (o2Ww / (1 + f * ome)).^(1/3); % cm, structural length, using 239 g wet mass
  L = (WwJO_23(:,1) / (1 + f * ome)).^(1/3); % cm, structural length, using measured wet mass
  pACSJGRD = p_ref * scaled_power(L, f, pars_power, l_b, l_p);
  pADG = pACSJGRD(:, [1 7 5]); % pADG(:,1) = 0; % exclude assim contribution
  JM = pADG * eta_O' * O2M;                     % mol/d, mineral fluxes
  X_gas = 24.06;  % gas correction factor, L/mol
  % EO = (- 1 * (JM(:,3) * X_gas) .* tempcorr(C2K(TO(:,1)), T_ref, pars_T))/24/60*1000; % L/day to ml/min
  EO_23 = (- 1 * (JM(:,3) * X_gas) .* TC_WO_23)/24*1000; % L/day to muL/h
  
  % temp = 28 C
  L = (WwJO_28(:,1) / (1 + f * ome)).^(1/3); % cm, structural length, using measured wet mass
  pACSJGRD = p_ref * scaled_power(L, f, pars_power, l_b, l_p);
  pADG = pACSJGRD(:, [1 7 5]); % pADG(:,1) = 0; % exclude assim contribution
  JM = pADG * eta_O' * O2M;                     % mol/d, mineral fluxes
  X_gas = 24.06;  % gas correction factor, L/mol
  EO_28 = (- 1 * (JM(:,3) * X_gas) .* TC_WO_28)/24*1000; % muL/h to ml/min

  % temp = 33 C
  L = (WwJO_33(:,1) / (1 + f * ome)).^(1/3); % cm, structural length, using measured wet mass
  pACSJGRD = p_ref * scaled_power(L, f, pars_power, l_b, l_p);
  pADG = pACSJGRD(:, [1 7 5]); % pADG(:,1) = 0; % exclude assim contribution
  JM = pADG * eta_O' * O2M;                     % mol/d, mineral fluxes
  X_gas = 24.06;  % gas correction factor, L/mol
  EO_33 = (- 1 * (JM(:,3) * X_gas) .* TC_WO_33)/24*1000; % muL/h to ml/min
  




  % pack to output
  prdData.tL_f = ELw_f;
  prdData.tL_m = ELw_m;
  prdData.LN = LN;
  prdData.LW_f = EWw_f;
  prdData.LW_m = EWw_m;
  prdData.Tab = aT_b2;
  
    %   prdData.TO = EO;
  prdData.WwJO_23 = EO_23;
  prdData.WwJO_28 = EO_28;
  prdData.WwJO_33 = EO_33;

end

    
function pACSJGRD = scaled_power(L, f, p, lb, lp)
  % created 2009/01/15 by Bas Kooijman
  
  %% Syntax
  % pACSJGRD = <../scaled_power.m *scaled_power*> (L, f, p, lb, lp)
  
  %% Description
  % Gets scaled powers assimilation, mobilisation, somatic maintenance, maturity maintenance,
  % growth, reproduction and dissipation as function of length.
  % Scaled powers are calculated on the assumption for maturity that food density is constant. 
  % The lengths in the first input must be ordered.
  %
  % Input
  %
  % * L: n-vector with lengths
  % * f: scalar with (constant) scaled functional response
  % * p: 9-vector with parameters: kap kapR g kJ kM LT v UHb UHp
  % * lb: scalar with scaled length at birth for f; if not existent: NaN
  % * lp: scalar with scaled length at puberty for f; if not existent: NaN  
  %
  % Output
  %
  % * pACSJGRD: (n,7)-matrix with scaled powers p_A, p_C, p_S, p_J, p_G, p_R, p_D / L_m^2 {p_Am}
  
  %% Remarks
  % The powers can become negative for shrinking of structure or maturity. 
  % The scaled powers are dimensionless by dividing the powers by {pAm} Lm2. 
  % The maturity value relates to the one for which f has been constant.
  % See function <scaled_power_j.html *scaled_power_j*> for metabolic acceleration.
  
  %% Example of use
  % See <../statistics.m *statistics*> and <../shpower.m *shpower*>

  % unpack parameters
  kap  = p(1);  % -, fraction allocated to growth + som maint
  kapR = p(2);  % -, fraction of reprod flux that is fixed in embryo reserve 
  g    = p(3);  % -, energy investment ratio
  kJ   = p(4);  % 1/d, maturity maint rate coeff
  kM   = p(5);  % 1/d, somatic maint rate coeff
  LT   = p(6);  % cm, heating length 
  v    = p(7);  % cm/d, energy conductance
  Hb   = p(8);  % d cm^2, scaled maturity at birth
  Hp   = p(9);  % d cm^2, scaled maturity at puberty

  Lm = v/ kM/ g; k = kJ/ kM;
  L = L(:);
  [L,ia,ic] = unique(L); % sort and pull out unique L values
  l = L/ Lm; lT = LT/ Lm; 

  if isnan(lb) && isnan(lp) % juvenile stage cannot be reached
    lT = 0 * l;
    assim = 0 * l;
    kapR = 0 * l;
  elseif isnan(lp) % adult stage cannot be reached
    lT = (l > lb) .* lT;
    assim = (l > lb);
    kapR = 0 * l;
  else % adult stage can be reached
    lT = (l > lb) .* lT;
    assim = (l > lb);
    kapR = kapR * (l > lp);
  end

  H = maturity_old(L, f, p);  % scaled maturities E_H/ {p_Am}
  uH = H *  g^2 * kM^3/ v^2;

  pA = assim * f .* l.^2;                    % assim
  pC = f .* l.^2 .* (g + l + lT) ./ (g + f); % mobilisation
  pS = kap * l.^2 .* (l + lT);               % somatic  maint
  pJ = k * uH;                               % maturity maint
  pG = kap * pC - pS;                        % growth
  pR = (1 - kap) * pC - pJ;                  % maturation/reproduction
  pD = pS + pJ + (1 - kapR) .* pR ;          % dissipation

  pACSJGRD = [pA(ic), pC(ic), pS(ic), pJ(ic), pG(ic), pR(ic), pD(ic)];
  
end
