-- dev.dev_ftp_action
-- dev.dev_ftp_comment
-- dev.dev_ftp_product
-- dev.dev_ftp_user

-- �ύ�׶Σ����û�2016-04-16��2016-04-20�Ƿ��µ�P�е���Ʒ

-- ������֤�����û�2016-04-09��2016-04-13�Ƿ��µ�P�е���Ʒ
-- ����ѵ���������ݣ�2016-03-01��2016-04-01
-- ����Ԥ���������ݣ�2016-03-01��2016-04-08
-- ����ѵ��������ݣ�2016-04-02��2016-04-06
-- �ύԤ���������ݣ�2016-03-01��2016-04-15

-- ȫ�����ݣ�����model_idȥ��
drop table if exists dev.dev_ftp_cc_action_unique;
create table dev.dev_ftp_cc_action_unique stored as orc as
select
	user_id,
	sku_id,
	time,
	type,
	cate,
	brand,
	min(to_date(time)) as dt
from
	dev.dev_ftp_action
where
	user_id <> 'user_id'
group by
	user_id,
	sku_id,
	time,
	type,
	cate,
	brand;

-- ����ѵ��������
-- ����ѵ���������ݣ�2016-03-01��2016-04-01
-- ����32��
drop table if exists dev.dev_ftp_cc_offline_train_x;
create table dev.dev_ftp_cc_offline_train_x stored as orc as
select
	user_id,

    32 as stat_day_cnt, -- ����ͳ������
	
	count(*) as record_cnt, -- ��¼��
	
	sum(case when type = 1 then 1 else 0 end)/32 as type1_cnt, -- type1��¼�������
	sum(case when type = 2 then 1 else 0 end)/32 as type2_cnt, -- type2��¼�������빺�ﳵ
	sum(case when type = 3 then 1 else 0 end)/32 as type3_cnt, -- type3��¼�������ﳵɾ��
	sum(case when type = 4 then 1 else 0 end)/32 as type4_cnt, -- type4��¼�����µ�
	sum(case when type = 5 then 1 else 0 end)/32 as type5_cnt, -- type5��¼������ע
	sum(case when type = 6 then 1 else 0 end)/32 as type6_cnt, -- type6��¼�������
	
	sum(case when cate = 4 then 1 else 0 end)/32 as cate4_cnt, -- cate4��¼��
	sum(case when cate = 5 then 1 else 0 end)/32 as cate5_cnt, -- cate5��¼��
	sum(case when cate = 6 then 1 else 0 end)/32 as cate6_cnt, -- cate6��¼��
	sum(case when cate = 7 then 1 else 0 end)/32 as cate7_cnt, -- cate7��¼��
	sum(case when cate = 8 then 1 else 0 end)/32 as cate8_cnt, -- cate8��¼��
	sum(case when cate = 9 then 1 else 0 end)/32 as cate9_cnt, -- cate9��¼��
	sum(case when cate = 10 then 1 else 0 end)/32 as cate10_cnt, -- cate10��¼��
	sum(case when cate = 11 then 1 else 0 end)/32 as cate11_cnt, -- cate11��¼��
	
	sum(case when dt = '2016-04-01' and cate = 6 and type = 1 then 1 else 0 end) as cate6_type1_last1day, -- ���1��cate6��type1��¼��
	sum(case when dt = '2016-04-01' and cate = 6 and type = 2 then 1 else 0 end) as cate6_type2_last1day, -- ���1��cate6��type2��¼��
	sum(case when dt = '2016-04-01' and cate = 6 and type = 3 then 1 else 0 end) as cate6_type3_last1day, -- ���1��cate6��type3��¼��
	sum(case when dt = '2016-04-01' and cate = 6 and type = 4 then 1 else 0 end) as cate6_type4_last1day, -- ���1��cate6��type4��¼��
	sum(case when dt = '2016-04-01' and cate = 6 and type = 5 then 1 else 0 end) as cate6_type5_last1day, -- ���1��cate6��type5��¼��
	sum(case when dt = '2016-04-01' and cate = 6 and type = 6 then 1 else 0 end) as cate6_type6_last1day, -- ���1��cate6��type6��¼��
	
	sum(case when dt = '2016-04-01' and cate = 5 and type = 1 then 1 else 0 end) as cate5_type1_last1day, -- ���1��cate5��type1��¼��
	sum(case when dt = '2016-04-01' and cate = 5 and type = 2 then 1 else 0 end) as cate5_type2_last1day, -- ���1��cate5��type2��¼��
	sum(case when dt = '2016-04-01' and cate = 5 and type = 3 then 1 else 0 end) as cate5_type3_last1day, -- ���1��cate5��type3��¼��
	sum(case when dt = '2016-04-01' and cate = 5 and type = 4 then 1 else 0 end) as cate5_type4_last1day, -- ���1��cate5��type4��¼��
	sum(case when dt = '2016-04-01' and cate = 5 and type = 5 then 1 else 0 end) as cate5_type5_last1day, -- ���1��cate5��type5��¼��
	sum(case when dt = '2016-04-01' and cate = 5 and type = 6 then 1 else 0 end) as cate5_type6_last1day, -- ���1��cate5��type6��¼��
	
	sum(case when dt between date_sub('2016-04-01', 2) and '2016-04-01' and cate = 6 and type = 1 then 1 else 0 end) as cate6_type1_last3day, -- ���3��cate6��type1��¼��
	sum(case when dt between date_sub('2016-04-01', 2) and '2016-04-01' and cate = 6 and type = 2 then 1 else 0 end) as cate6_type2_last3day, -- ���3��cate6��type2��¼��
	sum(case when dt between date_sub('2016-04-01', 2) and '2016-04-01' and cate = 6 and type = 3 then 1 else 0 end) as cate6_type3_last3day, -- ���3��cate6��type3��¼��
	sum(case when dt between date_sub('2016-04-01', 2) and '2016-04-01' and cate = 6 and type = 4 then 1 else 0 end) as cate6_type4_last3day, -- ���3��cate6��type4��¼��
	sum(case when dt between date_sub('2016-04-01', 2) and '2016-04-01' and cate = 6 and type = 5 then 1 else 0 end) as cate6_type5_last3day, -- ���3��cate6��type5��¼��
	sum(case when dt between date_sub('2016-04-01', 2) and '2016-04-01' and cate = 6 and type = 6 then 1 else 0 end) as cate6_type6_last3day, -- ���3��cate6��type6��¼��
	
	count(distinct case when cate = 6 and type = 1 then sku_id else null end)/32 as cate6_type1_sku_cnt, -- cate6��type1��Ʒ��
	count(distinct case when cate = 6 and type = 2 then sku_id else null end)/32 as cate6_type2_sku_cnt, -- cate6��type2��Ʒ��
	count(distinct case when cate = 6 and type = 3 then sku_id else null end)/32 as cate6_type3_sku_cnt, -- cate6��type3��Ʒ��
	count(distinct case when cate = 6 and type = 4 then sku_id else null end)/32 as cate6_type4_sku_cnt, -- cate6��type4��Ʒ��
	count(distinct case when cate = 6 and type = 5 then sku_id else null end)/32 as cate6_type5_sku_cnt, -- cate6��type5��Ʒ��
	count(distinct case when cate = 6 and type = 6 then sku_id else null end)/32 as cate6_type6_sku_cnt, -- cate6��type6��Ʒ��

	count(distinct case when cate = 5 and type = 1 then sku_id else null end)/32 as cate5_type1_sku_cnt, -- cate5��type1��Ʒ��
	count(distinct case when cate = 5 and type = 2 then sku_id else null end)/32 as cate5_type2_sku_cnt, -- cate5��type2��Ʒ��
	count(distinct case when cate = 5 and type = 3 then sku_id else null end)/32 as cate5_type3_sku_cnt, -- cate5��type3��Ʒ��
	count(distinct case when cate = 5 and type = 4 then sku_id else null end)/32 as cate5_type4_sku_cnt, -- cate5��type4��Ʒ��
	count(distinct case when cate = 5 and type = 5 then sku_id else null end)/32 as cate5_type5_sku_cnt, -- cate5��type5��Ʒ��
	count(distinct case when cate = 5 and type = 6 then sku_id else null end)/32 as cate5_type6_sku_cnt -- cate5��type6��Ʒ��
from
	dev.dev_ftp_cc_action_unique
where
	dt <= '2016-04-01'
group by
	user_id;
		
-- ����ѵ����Ǳ�
-- ����ѵ��������ݣ�2016-04-02��2016-04-06
drop table if exists dev.dev_ftp_cc_offline_train_y;
create table dev.dev_ftp_cc_offline_train_y stored as orc as
select
	user_id,
	max(sku_id) as sku_id
from
	dev.dev_ftp_cc_action_unique
where
	dt between '2016-04-02' and '2016-04-06' and
	cate = 6 and
	type = 4
group by
	user_id;
	
-- ������֤��Ǳ�
-- ������֤�����û�2016-04-09��2016-04-13�Ƿ��µ�P�е���Ʒ
drop table if exists dev.dev_ftp_cc_offline_verify_y;
create table dev.dev_ftp_cc_offline_verify_y stored as orc as
select
	user_id,
	max(sku_id) as sku_id
from
	dev.dev_ftp_cc_action_unique
where
	dt between '2016-04-09' and '2016-04-13' and
	cate = 6 and
	type = 4
group by
	user_id;

-- ����Ԥ��������
-- ����Ԥ���������ݣ�2016-03-01��2016-04-08
-- ����39��
drop table if exists dev.dev_ftp_cc_offline_prediction_x;
create table dev.dev_ftp_cc_offline_prediction_x stored as orc as
select
	user_id,

    39 as stat_day_cnt, -- ����ͳ������
	
	count(*) as record_cnt, -- ��¼��
	
	sum(case when type = 1 then 1 else 0 end)/39 as type1_cnt, -- type1��¼�������
	sum(case when type = 2 then 1 else 0 end)/39 as type2_cnt, -- type2��¼�������빺�ﳵ
	sum(case when type = 3 then 1 else 0 end)/39 as type3_cnt, -- type3��¼�������ﳵɾ��
	sum(case when type = 4 then 1 else 0 end)/39 as type4_cnt, -- type4��¼�����µ�
	sum(case when type = 5 then 1 else 0 end)/39 as type5_cnt, -- type5��¼������ע
	sum(case when type = 6 then 1 else 0 end)/39 as type6_cnt, -- type6��¼�������
	
	sum(case when cate = 4 then 1 else 0 end)/39 as cate4_cnt, -- cate4��¼��
	sum(case when cate = 5 then 1 else 0 end)/39 as cate5_cnt, -- cate5��¼��
	sum(case when cate = 6 then 1 else 0 end)/39 as cate6_cnt, -- cate6��¼��
	sum(case when cate = 7 then 1 else 0 end)/39 as cate7_cnt, -- cate7��¼��
	sum(case when cate = 8 then 1 else 0 end)/39 as cate8_cnt, -- cate8��¼��
	sum(case when cate = 9 then 1 else 0 end)/39 as cate9_cnt, -- cate9��¼��
	sum(case when cate = 10 then 1 else 0 end)/39 as cate10_cnt, -- cate10��¼��
	sum(case when cate = 11 then 1 else 0 end)/39 as cate11_cnt, -- cate11��¼��
	
	sum(case when dt = '2016-04-08' and cate = 6 and type = 1 then 1 else 0 end) as cate6_type1_last1day, -- ���1��cate6��type1��¼��
	sum(case when dt = '2016-04-08' and cate = 6 and type = 2 then 1 else 0 end) as cate6_type2_last1day, -- ���1��cate6��type2��¼��
	sum(case when dt = '2016-04-08' and cate = 6 and type = 3 then 1 else 0 end) as cate6_type3_last1day, -- ���1��cate6��type3��¼��
	sum(case when dt = '2016-04-08' and cate = 6 and type = 4 then 1 else 0 end) as cate6_type4_last1day, -- ���1��cate6��type4��¼��
	sum(case when dt = '2016-04-08' and cate = 6 and type = 5 then 1 else 0 end) as cate6_type5_last1day, -- ���1��cate6��type5��¼��
	sum(case when dt = '2016-04-08' and cate = 6 and type = 6 then 1 else 0 end) as cate6_type6_last1day, -- ���1��cate6��type6��¼��
	
	sum(case when dt = '2016-04-08' and cate = 5 and type = 1 then 1 else 0 end) as cate5_type1_last1day, -- ���1��cate5��type1��¼��
	sum(case when dt = '2016-04-08' and cate = 5 and type = 2 then 1 else 0 end) as cate5_type2_last1day, -- ���1��cate5��type2��¼��
	sum(case when dt = '2016-04-08' and cate = 5 and type = 3 then 1 else 0 end) as cate5_type3_last1day, -- ���1��cate5��type3��¼��
	sum(case when dt = '2016-04-08' and cate = 5 and type = 4 then 1 else 0 end) as cate5_type4_last1day, -- ���1��cate5��type4��¼��
	sum(case when dt = '2016-04-08' and cate = 5 and type = 5 then 1 else 0 end) as cate5_type5_last1day, -- ���1��cate5��type5��¼��
	sum(case when dt = '2016-04-08' and cate = 5 and type = 6 then 1 else 0 end) as cate5_type6_last1day, -- ���1��cate5��type6��¼��
	
	sum(case when dt between date_sub('2016-04-08', 2) and '2016-04-08' and cate = 6 and type = 1 then 1 else 0 end) as cate6_type1_last3day, -- ���3��cate6��type1��¼��
	sum(case when dt between date_sub('2016-04-08', 2) and '2016-04-08' and cate = 6 and type = 2 then 1 else 0 end) as cate6_type2_last3day, -- ���3��cate6��type2��¼��
	sum(case when dt between date_sub('2016-04-08', 2) and '2016-04-08' and cate = 6 and type = 3 then 1 else 0 end) as cate6_type3_last3day, -- ���3��cate6��type3��¼��
	sum(case when dt between date_sub('2016-04-08', 2) and '2016-04-08' and cate = 6 and type = 4 then 1 else 0 end) as cate6_type4_last3day, -- ���3��cate6��type4��¼��
	sum(case when dt between date_sub('2016-04-08', 2) and '2016-04-08' and cate = 6 and type = 5 then 1 else 0 end) as cate6_type5_last3day, -- ���3��cate6��type5��¼��
	sum(case when dt between date_sub('2016-04-08', 2) and '2016-04-08' and cate = 6 and type = 6 then 1 else 0 end) as cate6_type6_last3day, -- ���3��cate6��type6��¼��
	
	count(distinct case when cate = 6 and type = 1 then sku_id else null end)/39 as cate6_type1_sku_cnt, -- cate6��type1��Ʒ��
	count(distinct case when cate = 6 and type = 2 then sku_id else null end)/39 as cate6_type2_sku_cnt, -- cate6��type2��Ʒ��
	count(distinct case when cate = 6 and type = 3 then sku_id else null end)/39 as cate6_type3_sku_cnt, -- cate6��type3��Ʒ��
	count(distinct case when cate = 6 and type = 4 then sku_id else null end)/39 as cate6_type4_sku_cnt, -- cate6��type4��Ʒ��
	count(distinct case when cate = 6 and type = 5 then sku_id else null end)/39 as cate6_type5_sku_cnt, -- cate6��type5��Ʒ��
	count(distinct case when cate = 6 and type = 6 then sku_id else null end)/39 as cate6_type6_sku_cnt, -- cate6��type6��Ʒ��
    
	count(distinct case when cate = 5 and type = 1 then sku_id else null end)/39 as cate5_type1_sku_cnt, -- cate5��type1��Ʒ��
	count(distinct case when cate = 5 and type = 2 then sku_id else null end)/39 as cate5_type2_sku_cnt, -- cate5��type2��Ʒ��
	count(distinct case when cate = 5 and type = 3 then sku_id else null end)/39 as cate5_type3_sku_cnt, -- cate5��type3��Ʒ��
	count(distinct case when cate = 5 and type = 4 then sku_id else null end)/39 as cate5_type4_sku_cnt, -- cate5��type4��Ʒ��
	count(distinct case when cate = 5 and type = 5 then sku_id else null end)/39 as cate5_type5_sku_cnt, -- cate5��type5��Ʒ��
	count(distinct case when cate = 5 and type = 6 then sku_id else null end)/39 as cate5_type6_sku_cnt -- cate5��type6��Ʒ��
from
	dev.dev_ftp_cc_action_unique
where
	dt <= '2016-04-08'
group by
	user_id;

-- ��ȡ����ѵ������
select
	A.*,
	B.sku_id,
	case when B.sku_id is not null then 1 else 0 end as label
from
	dev.dev_ftp_cc_offline_train_x A
left outer join
	dev.dev_ftp_cc_offline_train_y B
on
	A.user_id = B.user_id;
	
-- ��ȡ����Ԥ������
select * from dev.dev_ftp_cc_offline_prediction_x;

-- ��ȡ������֤����
select * from dev.dev_ftp_cc_offline_verify_y;
	
	
	
	
	
