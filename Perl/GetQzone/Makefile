CP_F = perl -MFile::Copy -e "File::Copy::cp(@ARGV)"
YML_OPT = -t 10
GET_QZONE = perl getqzone.pl
PLOT_QZONE = perl plotqzone.pl

Agent = 279005114
Cherry = 11854905
Clover = 312352111
Yabi = 386324429
#Kevin = 2161735
#Debenson = 281313252
#XiaoYu = 275506786
QianWei = 17927228

all: WebCache.pm archive.pl

WebCache.pm: ../GetMSN/WebCache.pm
	$(CP_F) $< $@

archive.pl: ../GetMSN/archive.pl
	$(CP_F) $< $@

gen: yml html

yml:
	$(GET_QZONE) $(YML_OPT) $(Agent)
	$(GET_QZONE) $(YML_OPT) $(Cherry)
	$(GET_QZONE) $(YML_OPT) $(Clover)
	$(GET_QZONE) $(YML_OPT) $(Yabi)
	$(GET_QZONE) $(YML_OPT) $(QianWei)

html: yml
	$(PLOT_QZONE) $(Agent)
	$(PLOT_QZONE) $(Cherry)
	$(PLOT_QZONE) $(Clover)
	$(PLOT_QZONE) $(Yabi)
	$(PLOT_QZONE) $(QianWei)

update:
	update $(Agent)
	update $(Cherry)
	update $(Clover)
	update $(Yabi)
	update $(QianWei)
