function [data, auxData, metaData, txtData, weights] = mydata_Iberolacerta_horvathi
%% set metaData
metaData.phylum     = 'Chordata'; 
metaData.class      = 'Reptilia'; 
metaData.order      = 'Squamata'; 
metaData.family     = 'Lacertidae';
metaData.species    = 'Iberolacerta_horvathi'; 
metaData.species_en = 'Horvaths rock lizard'; 

metaData.ecoCode.climate = {'Cfb','Csa','Cfa'}; 
metaData.ecoCode.ecozone = {'THp'};
metaData.ecoCode.habitat = {'0iTg'};
metaData.ecoCode.embryo  = {'Tt'};
metaData.ecoCode.migrate = {'TT'};
metaData.ecoCode.food    = {'biCi'};
metaData.ecoCode.gender  = {'Dg'};
metaData.ecoCode.reprod  = {'O'};

metaData.T_typical  = C2K(25); % K, body temp
metaData.data_0     = {'am'; 'Lb'; 'Lp'; 'Li'; 'Wwi'; 'Ri'}; 
metaData.data_1     = {'t-L'}; 

metaData.COMPLETE = 2.5; % using criteria of LikaKear2011

metaData.author   = {'Urban Dajcman','Urtzi Enriquez Urzelai', 'Anamarija Zagar'};    
metaData.date_subm = [2023 02 28];              
metaData.email    = {'Urban.Dajcman@nib.si'};            
metaData.address  = {'National Institute of Biology, Vecna pot 111, 1000 Ljubljana, Slovenia'};   

metaData.author_mod_1   = {'Urban Dajcman'};    
metaData.date_mod_1 = [2023 06 15];              
metaData.email    = {'Urban.Dajcman@nib.si'};            
metaData.address  = {'National Institute of Biology, Vecna pot 111, 1000 Ljubljana, Slovenia'};   

metaData.curator     = {'Starrlight Augustine'};
metaData.email_cur   = {'starrlight@tecnico.ulisboa.pt'}; 
metaData.date_acc    = [2023 06 17];

%% set data
% zero-variate data

data.ab = 44;  units.ab = 'd';    label.ab = 'age at birth';         bibkey.ab = 'LjubGlas2012';   
  temp.ab = C2K(24); units.temp.ab = 'K'; label.temp.ab = 'temperature';
data.tp = 3*365 ;     units.tp = 'd';    label.tp = 'time since birth at puberty';  bibkey.tp = 'BernDori2006';
  temp.tp = C2K(15); units.temp.tp = 'K'; label.temp.tp = 'temperature';
data.am = 9*365;  units.am = 'd';    label.am = 'life span';                bibkey.am = 'BernDori2006';   
  temp.am = C2K(15); units.temp.am = 'K'; label.temp.am = 'temperature';

data.Lb  = 2.48;  units.Lb  = 'cm';  label.Lb  = 'SVL at hatching';          bibkey.Lb  = 'LjubGlas2012'; 
data.Lp  = 4.90;   units.Lp  = 'cm';  label.Lp  = 'SVL at puberty for females'; bibkey.Lp  = 'Arribas2005';  
  comment.Lp = 'Based on other Iberolacerta species';
data.Lpm  = 4.50;    units.Lpm = 'cm';  label.Lpm = 'SVL at puberty for males';  bibkey.Lpm  = 'ArriGala2005'; 
  comment.Lpm = 'Based on other Iberolacerta species';
data.Li  = 6.770;     units.Li  = 'cm';  label.Li  = 'ultimate SVL for females';  bibkey.Li  = 'DajcZaga2022';  
  comment.Li = 'Dajcman_personal';
data.Lim  = 6.165;    units.Lim  = 'cm'; label.Lim  = 'ultimate SVL for males';   bibkey.Lim  = 'DajcZaga2022';  
  comment.Lim = 'Dajcman_personal';

data.Ww0 = 1.30;   units.Ww0 = 'g';  label.Ww0 = 'wet weight of eggs'; bibkey.Ww0 = 'BoscBout1998';
  comment.Ww0 = 'mass of eggs';
data.Wwb = 0.46;   units.Wwb = 'g';  label.Wwb = 'wet weight of hatchlings'; bibkey.Wwb = 'BoscBout1998';
  comment.Wwb = 'Body mass of hatchlings'; 
data.Wwi = 4.64;  units.Wwi = 'g';   label.Wwi = 'ultimate wet weight';       bibkey.Wwi = 'DajcZaga2022';
  comment.Wwi = 'weight females, Dajcman_personal' ;
data.Wwim = 4.96;   units.Wwim = 'g';  label.Wwim = 'ultimate wet males'; bibkey.Wwim = 'DajcZaga2022';
  comment.Wwim = 'ultimate weight males, Dajcman_personal';

data.Ri  = 5/365;   units.Ri  = '#/d'; label.Ri  = 'maximum reprod rate'; bibkey.Ri  = 'SpeyBeuk2016';   
  temp.Ri = C2K(15);   units.temp.Ri = 'K'; label.temp.Ri = 'temperature';
  comment.Ri = 'one clutch, based on Arribas for other Iberoalcerta';

% univariate data

% length-weight
data.LW_f = [ ... % SVL (cm), wet weight (g)
61.3	4.75
61.2	4.75
61.1	4.75
59.0	4.50
58.3	4.45
66.7	4.41
62.0	4.34
57.7	4.07
60.7	4.00
55.3	4.00
61.4	3.91
61.6	3.75
55.6	3.65
59.3	3.60
54.9	3.51
62.2	3.50
56.5	3.50
54.1	3.37
59.9	3.30
55.1	3.25
55.7	3.14
56.4	3.05
55.9	3.00
51.4	3.00
51.4	3.00
50.5	2.76
59.1	2.75
56.3	2.50
54.3	2.50
50.7	2.50
58.3	2.40
56.9	2.30
57.1	2.25];
data.LW_f(:,1) = data.LW_f(:,1) /10 ; % convert mm to cm
units.LW_f  = {'cm', 'g'};  label.LW_f = {'SVL', 'wet weight', 'female'};  
bibkey.LW_f = 'DajcZaga2022';
comment.LW_f = 'Dajcman_personal';
% males
data.LW_m = [ ... % SVL (cm), wet weight (g)
59.0	5.27
58.4	5.00
57.4	5.00
56.1	4.80
59.4	4.75
58.2	4.75
56.6	4.75
54.6	4.75
55.4	4.68
61.4	4.50
61.1	4.50
59.8	4.50
58.9	4.50
56.6	4.50
56.6	4.50
57.0	4.45
58.0	4.25
59.2	4.07
53.9	4.00
58.4	4.00
58.3	4.00
57.4	4.00
57.1	4.00
56.0	4.00
55.2	4.00
54.2	4.00
54.2	4.00
53.7	4.00
52.9	4.00
52.6	4.00
58.4	3.97
53.6	3.93
55.3	3.83
51.6	3.81
54.6	3.75
54.1	3.75
53.3	3.75
50.8	3.75
52.1	3.57
54.6	3.53
60.4	3.50
55.8	3.50
55.0	3.50
54.9	3.50
54.4	3.50
53.2	3.50
52.2	3.50
48.5	3.50
51.8	3.47
57.9	3.25
53.5	3.25
53.3	3.25
53.2	3.25
53.2	3.25
53.2	3.25
52.0	3.25
51.5	3.25
51.4	3.25
50.3	3.25
49.7	3.25
49.7	3.25
49.7	3.25
54.7	3.19
54.1	3.17
59.4	3.00
53.6	3.00
52.9	3.00
51.6	3.00
51.2	3.00
51.1	3.00
50.0	3.00
49.0	3.00
48.8	3.00
48.8	3.00
48.1	3.00
50.6	2.93
47.5	2.77
54.5	2.75
52.5	2.75
51.9	2.75
50.8	2.75
48.8	2.75
48.1	2.75
53.8	2.73
57.1	2.50
54.1	2.50
52.4	2.50
51.0	2.50
51.0	2.50
50.1	2.50
49.1	2.50
48.0	2.50
57.1	2.45
53.9	2.45
47.6	2.44
55.6	2.40
54.9	2.40
53.4	2.40
54.9	2.35
55.5	2.30
54.9	2.30
54.7	2.30
53.5	2.30
53.1	2.30
51.4	2.30
51.0	2.25
46.9	2.24
52.9	2.20
51.3	2.20
50.2	2.20
48.9	2.15
48.2	2.10
47.3	2.05
53.9	2.00
51.0	2.00
50.3	2.00
47.4	2.00
47.2	2.00];
data.LW_m(:,1) = data.LW_m(:,1) /10 ; % convert mm to cm
units.LW_m  = {'cm', 'g'};  label.LW_m = {'SVL', 'wet weight', 'male'};  
bibkey.LW_m = 'DajcZaga2022';
comment.LW_m = 'Dajcman_personal';


% SVL - yearly fecundity
data.LN = [ ... 
49.00 2.00
50.98 2.00
52.95 2.00
53.45 2.00
54.99 2.00
55.42 2.00
55.95 2.00
56.91 3.00
58.95 3.00
59.44 3.00
60.43 3.00
60.95 3.00
61.45 3.00
61.97 3.00
63.95 3.00
64.50 3.00
67.00 3.00
67.99 3.00
68.98 4.00
67.94 4.00
66.98 4.00
65.47 4.00
64.94 4.00
62.94 4.00
62.44 4.00
62.91 5.00];
data.LN(:,1) = data.LN(:,1) /10 ; % convert mm to cm
units.LN = {'cm', '#'}; label.LN = {'length', 'yearly fecundity'};
temp.LN = C2K(15); units.temp.LN = 'K'; label.temp.LN = 'temperature';
bibkey.LN = {'Luca1988'};

% weight-respiration data
% oxygen consumption (resting metabolic rate) , temperature , wet weight
OTW =[0.738866988	1.395651658	1.654849033	0.621376648	1.080596836	1.053951689	0.944842899	0.632423313	1.730892809	1.20692161	0.927943861	1.708889655	0.78810661	0.578108965	2.057304575	0.600606755	0.956158373	1.543631528	1.066543447	0.989294139	0.765356305	0.538884512	1.101717536	1.014475257	0.478512528	0.365635963	0.78970538	1.096448266	1.730953294	1.794580887	0.881207269	1.533750928	1.798583554	1.529679373	2.177241565	2.534468765	0.826090099	1.470967177	2.051590466	1.114605148	1.828277768	1.612494811	0.67226513	1.488940185	1.455875962	0.93919751	1.678505385	2.563284063	0.982696242	1.54360739	1.671872341	1.351104842	1.367891292	1.919855916	1.159485267	1.759506457	1.597545428	0.630928515	1.435956406	1.663254761	0.859019213	1.162960645	1.636779064	1.384777859	0.542131199	1.978282317	1.149742199	1.933432721	3.694441781	0.790589645	0.52815342	1.453849558	1.160132819	1.675955846	2.212449443	1.517386054	1.80057016	2.347074529	1.185044297	1.575812723	1.501217882	1.118699504	1.615379129	2.03386608	1.294041767	2.002039027	2.646115709	1.375978153	1.382282393	2.867803868	1.174668415	2.48012788	3.001258515	1.040681454	2.150396133	2.402224874	0.8062306	2.155972628	2.682951166	1.155567635	1.48688237	2.755403703	1.900761197	2.01021614	2.835022278	1.738488118	2.063744994	2.774554519	1.145256073	1.912459866	1.200784609	1.333547824	2.894388482	1.174255893	1.589855324	3.653445774	2.422654687	2.966485479	0.996191277	1.68604274	2.982630609	1.217998514	1.984340237	2.466903013	1.518993063	2.552333611	2.314974177	1.137661649	3.298468422	1.119408552	1.418345057	2.758019055	1.787454653	2.460487586	3.648328967	1.044279567	1.015560105	2.576354628	1.572086947	3.068976413	3.499599754	1.254402135	2.076561546	3.082566476	1.456305252	1.984850711	2.47280632	1.137698923	1.852764247	2.702071195	1.291036049	2.098819448	2.834414449	1.61178738	2.460910931	1.30501099	1.830051451	2.192462919	1.42381816	1.678547921	2.912268869	1.386526127	2.19143558	2.791833128	1.474280198	2.383764983	3.949694938
23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	23	28	33	23	28	33	28	33	23	28	33	23	28	33	23	28	33	23	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	33	23	28	23	28	33	23	28	33	23	28	33	23	28	33
1.3576	1.3576	1.3576	1.3857	1.3857	1.3857	1.4428	1.4428	1.4428	1.5795	1.5795	1.5795	1.6024	1.6024	1.6024	1.6481	1.6481	1.6481	1.6896	1.6896	1.6896	1.7287	1.7287	1.7287	1.9349	1.9349	1.9349	2.169	2.169	2.169	2.2569	2.2569	2.2569	2.3555	2.3555	2.3555	2.3975	2.3975	2.3975	2.5067	2.5067	2.5067	2.5145	2.5145	2.5145	2.6297	2.6297	2.6297	2.6528	2.6528	2.6528	2.6555	2.6555	2.6555	2.7044	2.7044	2.7044	2.8463	2.8463	2.8463	2.9099	2.9099	2.9099	2.9411	2.9411	2.9411	3.0066	3.0066	3.0066	3.0213	3.0213	3.0213	3.0361	3.0361	3.0361	3.0472	3.0472	3.0472	3.1419	3.1419	3.1419	3.19	3.19	3.19	3.2147	3.2147	3.2147	3.2865	3.2865	3.2865	3.4103	3.4103	3.4103	3.4152	3.4152	3.4152	3.4312	3.4312	3.4312	3.4962	3.4962	3.4962	3.5098	3.5098	3.5098	3.7786	3.7786	3.7786	3.8673	3.8673	4.1107	4.1107	4.1107	4.131	4.131	4.131	4.205	4.205	4.2312	4.2312	4.2312	4.2704	4.2704	4.2704	4.3346	4.3346	4.3346	4.399	4.399	4.6534	4.6534	4.6534	4.6845	4.6845	4.6845	4.6906	4.6906	4.6906	4.8966	4.8966	4.8966	4.9158	4.9158	4.9158	4.9537	4.9537	4.9537	4.9681	4.9681	4.9681	5.0112	5.0112	5.0112	5.091	5.091	5.1249	5.1249	5.1249	5.2259	5.2259	5.2259	5.383	5.383	5.383	6.0212	6.0212	6.0212
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
weights.ab = 1 * weights.ab;
weights.tp = 1 * weights.tp;
weights.LN = 1 * weights.LN;
weights.Ri = 1 * weights.Ri;
weights.Wwi = 5 * weights.Wwi;
weights.Wwim = 5 * weights.Wwim;
weights.WwJO_23 = weights.WwJO_23 * 0.5;
weights.WwJO_28 = weights.WwJO_28 * 0.5;
weights.WwJO_33 = weights.WwJO_33 * 0.5;

%% set pseudodata and respective weights
[data, units, label, weights] = addpseudodata(data, units, label, weights);

%% pack auxData and txtData for output
auxData.temp = temp;
txtData.units = units;
txtData.label = label;
txtData.bibkey = bibkey;
txtData.comment = comment;

%% Group plots
set1 = {'LW_f','LW_m'}; subtitle1 = {'Data for females, males'};
set2 = {'WwJO_23', 'WwJO_28', 'WwJO_33'}; subtitle2 = {'resting metabolic rate at 23, 28 and 33 C'};      
metaData.grp.sets = {set1,set2};
metaData.grp.subtitle = {subtitle1,subtitle2};

%% Discussion points
D1 = 'Males are assumed to differ from females by {p_Am} only';
D2 = 'Mod 1: added respiration data at three temperatures. Re-estimated. ';
metaData.discussion = struct('D1',D1, 'D2',D2);

%% Links
metaData.links.id_CoL = '3P7HK'; % Cat of Life
metaData.links.id_ITIS = '1155330'; % ITIS
metaData.links.id_EoL = '963387'; % Ency of Life
metaData.links.id_Wiki = 'Iberolacerta_horvathi'; % Wikipedia
metaData.links.id_ADW = 'Iberolacerta_horvathi'; % ADW
metaData.links.id_Taxo = '1685269'; % Taxonomicon
metaData.links.id_ReptileDB = 'genus=Iberolacerta&species=horvathi'; % ReptileDB

%% References
bibkey = 'Wiki'; type = 'Misc'; bib = ...
'howpublished = {\urlhttps://en.wikipedia.org/wiki/Iberolacerta_horvathi}}';
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'DajcZaga2022'; type = 'Article'; bib = [ ...  
'author = {Dajcman U. and Zagar A.}, ' ...
'title = {Unpublished data from personal archive}'];
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
bibkey = 'LjubGlas2012'; type = 'Article'; bib = [ ... 
'doi = {10.2298/ABS1202639L}, ' ...
'author = {Katarina Ljubisavljevi and Peter Glasnovi and Katja Kalan and Boris Krystufek}, ' ... 
'year = {2012}, ' ...
'title = {Female reproductive characteristics of the {H}orvathss rock lizard \emph(Iberolacerta horvathi) from {S}lovenia}, ' ... 
'journal = {Archives of Biological Sciences}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'ArriGala2005'; type = 'Article'; bib = [ ... 
'doi = {10.1163/1570756053993505}, ' ...
'author = {Oscar Arribas and Pedro Gal\''{a}n}, ' ... 
'year = {2005}, ' ...
'title = {Reproductive characteristics of the {P}yrenean high-mountain lizards: \emph(Iberolacerta aranica) ({A}rribas, 1993), \emph(I. aurelioi) ({A}rribas, 1994) and \emph(I. bonnali) ({L}antz, 1927)}, ' ... 
'journal = {Animal Biology}, ' ...
'volume = {55(2)}, ' ...
'pages = {163-109}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'SpeyBeuk2016'; type = 'Book'; bib = [ ... 
'author = {Jeroen Speybroeck and Wouter Beukema and Bobby Bok and Jan Van Der Voort and Ilian Velikov}, ' ... 
'year = {2016}, ' ...
'title = {Field Guide to the Amphibians and Reptiles of Britain and Europe, ' ... 
'publisher = {Bloomsbury Publishing}, ' ];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'Luca1988'; type = 'Thesis'; bib = [ ... 
'author = {Nina De Luca}, ' ... 
'year = {1988}, ' ...
'title = {Morfolo\v{s}ke i ekolo\v{s}ke karakteristike Velebitske gu\v{s}terice \emph{Lacerta horvathi} {M}ehely, 1904 ({L}acertidae, {R}eptilia) u Jugoslaviji}, ' ... 
'school = {Sveu\v{c}ili\v{s}te u Zagrebu}, ' ];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'BernDori2006'; type = 'Book'; bib = [ ... 
'author = {Franco Bernini and Giuliano Doria and Edoardo Razzetti and Roberto Sindaco}, ' ... 
'year = {2006}, ' ...
'title = {Atlas of Italian Amphibians and Reptiles / Atlante Degli Anfibi E Dei Rettili DItalia, ' ... 
'publisher = {Edizioni Polistampa}, ' ];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'BoschBout1998'; type = 'Article'; bib = [ ... 
'doi = {10.2307/1565456}, ' ...
'author = {Herman A. J. in den Bosch and Ron G. Bout}, ' ... 
'year = {1998}, ' ...
'title = {Relationships between Maternal Size, Egg Size, Clutch Size, and Hatchling Size in European Lacertid Lizards}, ' ... 
'journal = {Journal of herpetolog,}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'reptileDB'; type = 'Misc'; bib = ...
'howpublished = {\url{https://reptile-database.reptarium.cz/species?genus=Iberolacerta&species=horvathi}}';
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'Arribas2005'; type = 'Article'; bib = [ ... 
'doi = {10.1163/1570756053993505}, ' ...
'author = {Arribas, O. & Galán, P}, ' ... 
'year = {2005}, ' ...
'title = {Reproductive characteristics of the Pyrenean high-mountain lizards: Iberolacerta aranica (Arribas, 1993), I. aurelioi (Arribas, 1994) and I. bonnali (Lantz, 1927)}, ' ... 
'journal = {Animal biology}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];
%
bibkey = 'Zagar2018'; type = 'Article'; bib = [ ... 
'doi = {10.1111/ecog.03411}, ' ...
'author = {Anamarija Žagar, Miguel A. Carretero, Diana Marguč, Tatjana Simčič, Al Vrezec}, ' ... 
'year = {2018}, ' ...
'title = {A metabolic syndrome in terrestrial ectotherms with different elevational and distribution patterns}, ' ... 
'journal = {Ecography}'];
metaData.biblist.(bibkey) = ['''@', type, '{', bibkey, ', ' bib, '}'';'];


end
