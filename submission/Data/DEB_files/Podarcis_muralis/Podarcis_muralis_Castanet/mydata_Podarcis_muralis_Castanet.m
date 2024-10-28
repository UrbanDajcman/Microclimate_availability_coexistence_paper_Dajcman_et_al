function [data, auxData, metaData, txtData, weights] = mydata_Podarcis_muralis_Castanet
%% set metaData
metaData.phylum     = 'Chordata'; 
metaData.class      = 'Reptilia'; 
metaData.order      = 'Squamata'; 
metaData.family     = 'Lacertidae';
metaData.species    = 'Podarcis_muralis_Castanet'; 
metaData.species_en = 'Common wall lizard'; 

metaData.ecoCode.climate = {'Cfb', 'Csa', 'Cfa'}; %Should all climates where it lives be added?
metaData.ecoCode.ecozone = {'THp'};
metaData.ecoCode.habitat = {'0iTg'};
metaData.ecoCode.embryo  = {'Tt'};
metaData.ecoCode.migrate = {'TT'};
metaData.ecoCode.food    = {'biCi'};
metaData.ecoCode.gender  = {'Dg'};
metaData.ecoCode.reprod  = {'O'};

metaData.T_typical  = C2K(25); % K, body temp
metaData.data_0     = {'ab';'ap';'am'; 'Lb'; 'Lp'; 'Li'; 'Ww0'; 'Wwb'; 'Wwi'; 'Ri'}; 
metaData.data_1     = {'t-L'; 'L-Ww'; 'L-N'; 'T-ab'; 'Ww-JO_T'}; 

metaData.COMPLETE = 2.8; % using criteria of LikaKear2011

metaData.author   = {'Urban Dajcman', 'Urtzi Enriquez Urzelai', 'Anamarija \v{Z}agar'};    
metaData.date_subm = [2023 04 06];              
metaData.email    = {'Urban.Dajcman@nib.si'};            
metaData.address  = {'National Institute of Biology, Vecna pot 111, 1000 Ljubljana, Slovenia'};   

metaData.author_mod_1   = {'Urban Dajcman'};    
metaData.date_mod_1 = [2023 06 15];              
metaData.email_mod_1    = {'Urban.Dajcman@nib.si'};            
metaData.address_mod_1  = {'National Institute of Biology, Vecna pot 111, 1000 Ljubljana, Slovenia'};   

metaData.curator     = {'Starrlight Augustine'};
metaData.email_cur   = {'starrlight@tecnico.ulisboa.pt'}; 
metaData.date_acc    = [2023 06 17];

%% set data
% zero-variate data

data.ab = 65.9;  units.ab = 'd';    label.ab = 'age at birth';         bibkey.ab = 'DammBauw1992';   
  temp.ab = C2K(23); units.temp.ab = 'K'; label.temp.ab = 'temperature';  
data.tp = 22*30;     units.tp = 'd';    label.tp = 'time since birth at puberty';  bibkey.tp = 'CastRoch1981';
  temp.tp = C2K(15); units.temp.tp = 'K'; label.temp.tp = 'temperature';
  comment.tp = '35 months according to MolinaRodriguez2004';
data.am = 10*365;  units.am = 'd';    label.am = 'life span';                bibkey.am = 'Cast1994';   
  temp.am = C2K(15); units.temp.am = 'K'; label.temp.am = 'temperature'; 

data.Lb  = 2.41;  units.Lb  = 'cm';  label.Lb  = 'SVL at hatching';          bibkey.Lb  = 'BoscBout1998'; 
data.Lp  = 4.42;   units.Lp  = 'cm';  label.Lp  = 'SVL at puberty for females'; bibkey.Lp  = 'CastRoch1981';  
data.Lpm  = 4.99;    units.Lpm = 'cm';  label.Lpm = 'SVL at puberty for males';  bibkey.Lpm  = 'CastRoch1981';
data.Li  = 6.683;     units.Li  = 'cm';  label.Li  = 'ultimate SVL for females';  bibkey.Li  = 'Dajc2022'; 
  comment.Li = 'Dajcman_personal'; 
data.Lim  = 6.223;    units.Lim  = 'cm'; label.Lim  = 'ultimate SVL for males';   bibkey.Lim  = 'Dajc2022';  
  comment.Lim = 'Dajcman_personal';

data.Ww0 = 0.98;   units.Ww0 = 'g';  label.Ww0 = 'wet weight of eggs'; bibkey.Ww0 = 'InDenBosch1998';
data.Wwb = 0.36;   units.Wwb = 'g';  label.Wwb = 'wet weight of hatchlings'; bibkey.Wwb = 'InDenBosch1998';
data.Wwi = 5.078;  units.Wwi = 'g';   label.Wwi = 'ultimate wet weight';       bibkey.Wwi = 'Dajc2022';
  comment.Wwi = 'Mean body mass, Dajcman_personal';
data.Wwim = 5.95;   units.Wwim = 'g';  label.Wwim = 'ultimate wet males'; bibkey.Wwim = 'Dajc2022';
  comment.Wwim = 'Mean body mass, Dajcman_personal';

data.Ri  = 10/365;   units.Ri  = '#/d'; label.Ri  = 'maximum reprod rate'; bibkey.Ri  = 'JiBran2000';   
  temp.Ri = C2K(15);   units.temp.Ri = 'K'; label.temp.Ri = 'temperature';
  comment.Ri = '2 to 3 clutches, ranging from 3-11 for first clutch and 2-6 for second and third';

% univariate data
% time-length
data.tL_f = [ ... % time since 1 Jan (mth), SVL (mm)
10	33.40
8   34.60
9   37.20
8   37.30
22	37.80
20	39.20
21	40.30
21	41.80
22	42.70
23	44.30
20	45.30
23	48.00
24	48.20
24	49.10
22	49.60
34	50.60
36	51.40
34	52.00
33	56.50
34	57.10
45	54.50
45	56.30
46	56.60
44	57.10
45	57.30
46	58.80
45	59.10];
data.tL_f(:,1) = data.tL_f(:,1) * 30.5 ; % convert time in months to days
data.tL_f(:,2) = data.tL_f(:,2)/ 10 ; % convert mm to cm
units.tL_f  = {'d', 'cm'};  label.tL_f = {'time since birth', 'SVL'};  
temp.tL_f   = C2K(15);  units.temp.tL_f = 'K'; label.temp.tL_f = 'temperature';
bibkey.tL_f = 'CastRoch1981';
comment.tL_f = 'Data for females';
%
data.tL_m = [ ... % time since 1 Jan (mnth), SVL (mm)
10	32.90
10	35.00
10	36.20
21	43.70
26	51.40
23	51.50
23	53.00
34	52.00
34	52.10
34	52.20
33	53.80
36	54.30
38	54.30
38	54.30
36	55.00
35	55.30
33	55.40
33	56.60
34	57.00
33	57.60
38	57.70
45	57.50
45	57.50
46	57.60
46	57.80
45	58.10
46	58.40
46	58.50
46	59.30
45	60.60
58	58.70
58	60.00
58	60.40
57	63.40];
data.tL_m(:,1) = data.tL_m(:,1) * 30.5 ; % convert time in months to days
data.tL_m(:,2) = data.tL_m(:,2) / 10 ; % convert mm to cm
units.tL_m  = {'d', 'cm'};  label.tL_m = {'time since birth', 'SVL'};  
temp.tL_m   = C2K(15);  units.temp.tL_m = 'K'; label.temp.tL_m = 'temperature';
bibkey.tL_m = 'CastRoch1981';
comment.tL_m = 'Data for males';

% length-weight
data.LW_f = [ ... % SVL (mm), wet weight (g)
64.1	5.50
60.6	4.75
57.5	4.75
56.1	4.50
59.0	4.25
59.3	4.25
60.9	4.18
59.1	4.08
56.9	4.01
59.8	4.00
56.9	4.00
53.0	4.00
54.7	3.99
58.6	3.98
51.8	3.76
56.7	3.75
49.2	3.75
57.6	3.63
52.8	3.52
50.2	3.50
59.2	3.50
53.9	3.50
57.1	3.50
55.6	3.50
51.9	3.50
54.0	3.50
54.7	3.32
51.1	3.23
50.0	3.04
54.0	3.01
50.7	2.80
52.1	2.75];
data.LW_f(:,1) = data.LW_f(:,1) /10 ; % convert mm to cm
units.LW_f  = {'cm', 'g'};  label.LW_f = {'SVL', 'wet weight', 'female'};  
bibkey.LW_f = 'Dajc2022';
comment.LW_f = 'Dajcman_personal';
% 
data.LW_m = [ ... % SVL (m), wet weight (g)
61.4	6.25
61.8	6.25
59.8	6.00
62.6	5.75
59.1	5.50
59.5	5.40
59.3	5.34
58.1	5.31
59.2	5.25
56.5	5.25
59.2	5.23
57.4	5.00
61.1	5.00
56.9	4.91
57.6	4.78
61.0	4.75
60.8	4.69
57.4	4.67
56.5	4.64
58.2	4.61
61.7	4.52
58.0	4.50
60.5	4.50
53.4	4.50
58.3	4.50
56.8	4.50
55.2	4.50
54.9	4.50
60.1	4.49
57.6	4.47
58.9	4.37
57.4	4.35
55.1	4.25
55.7	4.25
57.4	4.20
52.3	4.13
58.3	4.00
56.9	4.00
52.2	4.00
55.6	4.00
55.4	4.00
54.9	4.00
58.2	4.00
56.1	3.88
52.9	3.81
53.1	3.75
54.1	3.75
54.4	3.75
51.9	3.70
54.0	3.55
50.8	3.50
52.1	3.50
51.1	3.50
49.5	3.41
54.9	3.34
50.0	3.27
50.5	3.25
55.2	3.25
49.5	3.25
49.8	3.25
52.3	3.21
47.9	3.00
50.7	3.00
50.6	3.00
51.1	3.00
47.5	2.75
49.0	2.75
47.2	2.71
51.7	2.50];
data.LW_m(:,1) = data.LW_m(:,1) /10 ; % convert mm to cm
units.LW_m  = {'cm', 'g'};  label.LW_m = {'SVL', 'wet weight', 'male'};  
bibkey.LW_m = 'Dajc2022';
comment.LW_m = 'Dajcman_persona';

% SVL - yearly fecundity
data.LN = [ ...  SVL (mm), fecundity (#)
51.10	3
54.72	3
60.15	3
58.37	4
57.40	4
55.33	4
54.56	5
58.17	5
59.34	5
60.24	5
59.53	6
58.21	6
57.82	6
57.59	6
54.94	8
55.30	9
53.88	9
59.50	7
60.99	7
61.53	7
60.89	8
59.98	8
61.86	9
64.67	10
66.19	10
64.89	12
62.50	11
60.34	10
59.31	9
58.34	9
57.63	9
58.27	10
58.95	10
59.82	11
60.76	11
62.28	12
59.98	12
56.88	11
61.63	13
60.82	14
61.89	15
62.44	15
62.25	16
63.83	16
63.89	17
68.16	17
57.43	17];
data.LN(:,1) = data.LN(:,1) /10 ; % convert mm to cm
units.LN = {'cm', '#'}; label.LN = {'length', 'yearly fecundity'};
temp.LN = C2K(20); units.temp.LN = 'K'; label.temp.LN = 'temperature';
bibkey.LN = {'JiBran2000'};

% temp dev time
data.Tab = [23  29; 65.9 32.6]'; units.Tab  = {'C','d'};  label.Tab  = {'temperature','development time'};  
bibkey.Tab  = 'HenaMeyl2012';

% weight-respiration data
% oxygen consumption (resting metabolic rate) , temperature , wet weight
OTW =[0.571250811	0.379779258	0.614188563	0.741345492	0.359907477	0.626720548	0.739551322	0.605896114	1.079296662	1.791026481	0.989849339	1.104430444	0.450588376	1.026453037	1.123465605	1.299588313	1.081126136	2.224400321	0.449706527	0.86219822	1.085288603	0.520822374	0.933270191	1.204070506	1.282372547	1.818635312	1.71559411	1.077666732	2.519381324	1.775783043	2.32381453	2.885081753	1.197084416	1.95000659	2.43733671	1.584899824	1.792653443	2.031436126	1.051586922	1.90571076	2.313958016	1.347620455	1.847597465	1.770321142	2.301911423	4.360668853	0.790230288	1.98333328	2.317974057	1.304990817	3.843483281	1.413720349	1.703129445	2.597545182	1.544521017	2.511893064	3.372288101	1.672562131	4.441765449	1.548385838	1.357580376	1.30421831	1.514179835	1.986823442	2.82076745	1.86905312	1.9351931	2.902077075	1.595288171	2.399856692	2.365635573	1.266197544	2.340969247	2.644380905	1.38216369	2.21220215	5.957764406	1.492895934	1.489171419	0.965131771	2.310851399	2.501940252	1.44586108	2.612242148	1.390433278	2.307709549	1.483278326	1.876372147	2.800322936	4.637889442	1.409307413	4.675536493	2.115440095	2.621297278	2.550384623	3.125604667	3.41562842	5.254650097	5.037554916	1.253305974	2.493343989	2.880948333	1.827515136	2.527574271	2.885062547	1.603702682	2.608832946	5.216873239	1.603452061	1.81097386	1.95000659	3.759616545	3.095153828	3.754409107	3.899058085	2.167379894	2.477540005	2.871852762	1.828106678	2.361540244	2.932034775	1.554421793	2.878818731	2.369709048	1.631275873	1.776755934	2.580636945	1.464549753	1.43739772	1.840257429	1.168771381	2.125236345	2.392950176	1.564963845	1.925057564	3.793546969	1.364878815	1.960548239	1.834980939	1.375925612	0.665933574	2.302828641	1.400863682	3.487025713	1.453782943	2.319615492	3.212007507	1.360960807	1.671304926	2.486023724	1.4544438	2.141218689	2.975886916	1.802307853	3.028188212	2.488917038	2.121896459	2.709346704	2.652523833	2.460607283	2.852225113	1.484904679	2.208761141	2.602097168	1.665323125	4.069945617	3.252002829	1.980029336	2.233048385	2.728143169	2.410756604	3.42640193	3.792627447	2.096050676	2.631220057	2.879249295	1.51100845	2.978939538	3.072227302	1.279514227	3.031061053	3.463348948	1.696526908	2.842809494	3.581808959	2.586689161	3.181643586	2.565442753	3.96471843	3.787613229	3.235629546	3.44866357	3.945038977
23	23	28	33	23	28	33	23	28	33	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	23	28	33	23	28	33	23	28	33	23	28	33	23	28	23	28	33	23	28	33	23	33	23	28	33	23	28	33	23	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	33	23	28	33	23	33	23	28	33	23	28	33	23	28	33	23	23	28	28	33	33	23	28	33	23	28	33	23	28	33	23	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	23	28	33	23	28	33
1.43	1.18	1.18	1.18	1.25	1.25	1.25	1.43	1.43	1.43	1.43	1.43	1.51	1.51	1.51	1.68	1.68	1.68	1.86	1.86	1.86	2.01	2.01	2.01	2.52	2.52	2.52	2.73	2.73	3.07	3.07	3.07	3.16	3.16	3.16	3.18	3.18	3.18	3.40	3.40	3.40	3.57	3.57	3.57	3.57	3.57	3.60	3.60	3.60	3.65	3.65	3.70	3.70	3.70	3.75	3.75	3.75	3.83	3.83	3.86	3.86	3.86	3.86	3.86	3.86	3.87	3.87	3.87	3.90	3.90	3.90	3.92	3.92	3.92	4.08	4.08	4.08	4.08	4.08	4.10	4.10	4.10	4.13	4.13	4.19	4.19	4.19	4.20	4.20	4.20	4.21	4.21	4.21	4.21	4.21	4.21	4.21	4.21	4.21	4.21	4.21	4.21	4.22	4.22	4.22	4.27	4.27	4.27	4.28	4.31	4.31	4.31	4.33	4.33	4.33	4.35	4.35	4.35	4.35	4.35	4.35	4.35	4.35	4.35	4.42	4.42	4.42	4.44	4.44	4.44	4.54	4.54	4.54	4.56	4.56	4.56	4.56	4.56	4.56	4.60	4.60	4.60	4.61	4.61	4.73	4.73	4.73	4.73	4.73	4.73	4.80	4.80	4.80	4.81	4.81	4.81	4.81	4.81	4.81	4.91	4.91	4.92	4.92	4.92	4.92	4.92	4.92	4.93	4.93	4.93	4.95	4.95	4.95	5.01	5.01	5.01	5.01	5.01	5.01	5.05	5.05	5.05	5.16	5.16	5.16	5.19	5.19	5.24	5.24	5.24	5.29	5.29	5.29
];
temp23 = find(OTW(2,:)==23); temp28 = find(OTW(2,:)==28); temp33 = find(OTW(2,:)==33);
data.WwJO_23 = [OTW(3,temp23)',OTW(1,temp23)'];
units.WwJO_23   = {'g', 'muLO2/h'};  label.WwJO_23 = {'wet weight', 'resting metabolic rate', '23 C'};  
bibkey.WwJO_23 = 'Zagar2018'; temp.WwJO_23 = C2K(23); units.temp.WwJO_23 = 'K'; label.temp.WwJO_23 = 'temperature';

data.WwJO_28 = [OTW(3,temp28)',OTW(1,temp28)'];
units.WwJO_28   = {'g', 'muLO2/h'};  label.WwJO_28 = {'wet weight', 'resting metabolic rate', '28 C'};  
bibkey.WwJO_28 = 'Zagar2018'; temp.WwJO_28 = C2K(28); units.temp.WwJO_28 = 'K'; label.temp.WwJO_28 = 'temperature';

data.WwJO_33 = [OTW(3,temp33)',OTW(1,temp33)'];
units.WwJO_33   = {'g', 'muLO2/h'};  label.WwJO_33 = {'wet weight', 'resting metabolic rate', '33 C'};  
bibkey.WwJO_33 = 'Zagar2018'; temp.WwJO_33 = C2K(33); units.temp.WwJO_33 = 'K'; label.temp.WwJO_33 = 'temperature';

%% set weights for all real data
weights = setweights(data, []);
weights.Lb = 0.5 * weights.Lb;
weights.Lp = 0.5 * weights.Lp;
weights.Lpm = 0.5 * weights.Lpm;
weights.Tab = 5 * weights.Tab;
weights.Wwi = 5 * weights.Wwi;
weights.Wwim = 5 * weights.Wwim;

weights.WwJO_23 = weights.WwJO_23 * 0.2;
weights.WwJO_28 = weights.WwJO_28 * 0.2;
weights.WwJO_33 = weights.WwJO_33 * 0.2;

%% set pseudodata and respective weights
[data, units, label, weights] = addpseudodata(data, units, label, weights);

%% pack auxData and txtData for output
auxData.temp = temp;
txtData.units = units;
txtData.label = label;
txtData.bibkey = bibkey;
txtData.comment = comment;

%% Group plots
set1 = {'tL_f','tL_m'}; subtitle1 = {'Data for females, males'};
set2 = {'LW_f','LW_m'}; subtitle2 = {'Data for females, males'}; 
set3 = {'WwJO_23', 'WwJO_28', 'WwJO_33'}; subtitle3 = {'resting metabolic rate at 23, 28 and 33 C'}; 

metaData.grp.sets = {set1,set2, set3};
metaData.grp.subtitle = {subtitle1,subtitle2, subtitle3};

%% Discussion points
D1 = 'Males are assumed to differ from females by {p_Am} only';
D2 = 'mod_1: added respiration data at three temperatures.';
metaData.discussion = struct('D1',D1, 'D2',D2);

%% Links
metaData.links.id_CoL = '4KPJ3 '; % Cat of Life
metaData.links.id_ITIS = '564582'; % ITIS
metaData.links.id_EoL = '47045459'; % Ency of Life
metaData.links.id_Wiki = 'Podarcis_muralis'; % Wikipedia
metaData.links.id_ADW = 'Podarcis_muralis'; % ADW
metaData.links.id_Taxo = '93099'; % Taxonomicon
metaData.links.id_ReptileDB = 'genus=Podarcis&species=muralis'; % ReptileDB
metaData.links.id_AnAge = 'Podarcis_muralis'; % AnAge

%% References
bibkey = 'Wiki'; type = 'Misc'; bib = ...
'howpublished = {\url{http://en.wikipedia.org/wiki/Podarcis_muralis}}';
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'Dajc2022'; type = 'Article'; bib = [ ...  
'author = {Dajcman U. and \v{Z}agar A.}, ' ...
'title = {Unpublished data}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'Kooy2010'; type = 'Book'; bib = [ ...  % used in setting of chemical parameters and pseudodata
'author = {Kooijman, S.A.L.M.}, ' ...
'year = {2010}, ' ...
'title  = {Dynamic Energy Budget theory for metabolic organisation}, ' ...
'publisher = {Cambridge Univ. Press, Cambridge}, ' ...
'pages = {Table 4.2 (page 150), 8.1 (page 300)}, ' ...
'howpublished = {\url{../../../bib/Kooy2010.html}}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'DammBauw1992'; type = 'Article'; bib = [ ... 
'doi = {}, ' ...
'author = {Raoul Van Damme and Dirk Bauwens and Florentino Bra\~{n}a and Rudolf F. Verheyen}, ' ... 
'year = {1992}, ' ...
'title = {Incubation Temperature Differentially Affects Hatching Time, Egg Survival, and Hatchling Performance in the Lizard \emph{Podarcis muralis}}, ' ...
'journal = {Herpetologica}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'CastRoch1981'; type = 'Article'; bib = [ ... 
'doi = {}, ' ...
'author = {Jacques Castanet and E. Roche}, ' ... 
'year = {1992}, ' ...
'title = {Determination de lage le lezard des murailles, \emph{Lacerta muralis} ({L}aurenti, 1768) au moyen de la squelettochronologie}, ' ...
'journal = {Revue suisse Zool.}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'Cast1994'; type = 'Article'; bib = [ ... 
'doi = {10.1159/000213586}, ' ...
'author = {Jacques Castanet}, ' ... 
'year = {1994}, ' ...
'title = {Age estimation and longevity in reptiles}, ' ...
'journal = {Gerontology}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'JiBran2000'; type = 'Article'; bib = [ ... 
'doi = {}, ' ...
'author = {Xiang Ji and Florentino Brana}, ' ... 
'year = {2000}, ' ...
'title = {Among Clutch Variation in Reproductive Output and Egg Size in the Wall Lizard \emph(Podarcis muralis) from a Lowland Population of Northern {S}pain}, ' ...
'journal = {Journal of Herpetology}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'BoscBout1998'; type = 'Article'; bib = [ ... 
'doi = {10.2307/1565456}, ' ...
'author = {Herman A. J. in den Bosch and Ron G. Bout}, ' ... 
'year = {1998}, ' ...
'title = {Relationships between Maternal Size, Egg Size, Clutch Size, and Hatchling Size in {E}uropean Lacertid Lizards}, ' ... 
'journal = {Journal of Herpetology}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'HenaMeyl2012'; type = 'Article'; bib = [ ... 
'doi = {10.1111/j.1095-8312.2012.02005.x}, ' ...
'author = {Maxime le Henanff and Sandrine Meylan and Olivier Lourdais}, ' ... 
'year = {2012}, ' ...
'title = {The sooner the better: reproductive phenology drives ontogenetic trajectories in a temperate squamate \emph{Podarcis muralis}}, ' ... 
'journal = {Biological Journal of the Linnean Society}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'Zagar2018'; type = 'Article'; bib = [ ... 
'doi = {10.1111/ecog.03411}, ' ...
'author = {Anamarija Žagar, Miguel A. Carretero, Diana Marguč, Tatjana Simčič, Al Vrezec}, ' ... 
'year = {2018}, ' ...
'title = {A metabolic syndrome in terrestrial ectotherms with different elevational and distribution patterns}, ' ... 
'journal = {Ecography}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'InDenBosch1998'; type = 'Article'; bib = [ ... 
'doi = {10.2307/1565456}, ' ...
'author = {In den Bosch, H.A.J. & Bout, R.G}, ' ... 
'year = {1998}, ' ...
'title = {Relationships between maternal size, egg size, clutch size, and hatchling size in European lacertid lizards}, ' ... 
'journal = {Journal of Herpetology}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];

