-- dev.dev_ftp_action
-- dev.dev_ftp_comment
-- dev.dev_ftp_product
-- dev.dev_ftp_user

-- �ύ�׶Σ����û�2016-04-16��2016-04-20�Ƿ��µ�P�е���Ʒ
-- �ύģ��ѵ���������ݣ�2016-03-01��2016-04-08
-- �ύģ��ѵ��������ݣ�2016-04-09��2016-04-13
-- �ύģ��ѵ��Ԥ�����ݣ�2016-03-01��2016-04-15

-- �ύѵ��������
-- �ύѵ���������ݣ�2016-03-01��2016-04-08
-- ����39��
drop table if exists dev.dev_ftp_cc_online_train_x;
create table dev.dev_ftp_cc_online_train_x stored as orc as
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
		
-- �ύѵ����Ǳ�
-- �ύѵ��������ݣ�2016-04-09��2016-04-13
drop table if exists dev.dev_ftp_cc_online_train_y;
create table dev.dev_ftp_cc_online_train_y stored as orc as
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
	
-- �ύԤ��������
-- �ύԤ���������ݣ�2016-03-01��2016-04-15
-- ����46��
drop table if exists dev.dev_ftp_cc_online_prediction_x;
create table dev.dev_ftp_cc_online_prediction_x stored as orc as
select
	user_id,

    46 as stat_day_cnt, -- ����ͳ������
	
	count(*) as record_cnt, -- ��¼��
	
	sum(case when type = 1 then 1 else 0 end)/46 as type1_cnt, -- type1��¼�������
	sum(case when type = 2 then 1 else 0 end)/46 as type2_cnt, -- type2��¼�������빺�ﳵ
	sum(case when type = 3 then 1 else 0 end)/46 as type3_cnt, -- type3��¼�������ﳵɾ��
	sum(case when type = 4 then 1 else 0 end)/46 as type4_cnt, -- type4��¼�����µ�
	sum(case when type = 5 then 1 else 0 end)/46 as type5_cnt, -- type5��¼������ע
	sum(case when type = 6 then 1 else 0 end)/46 as type6_cnt, -- type6��¼�������
	
	sum(case when cate = 4 then 1 else 0 end)/46 as cate4_cnt, -- cate4��¼��
	sum(case when cate = 5 then 1 else 0 end)/46 as cate5_cnt, -- cate5��¼��
	sum(case when cate = 6 then 1 else 0 end)/46 as cate6_cnt, -- cate6��¼��
	sum(case when cate = 7 then 1 else 0 end)/46 as cate7_cnt, -- cate7��¼��
	sum(case when cate = 8 then 1 else 0 end)/46 as cate8_cnt, -- cate8��¼��
	sum(case when cate = 9 then 1 else 0 end)/46 as cate9_cnt, -- cate9��¼��
	sum(case when cate = 10 then 1 else 0 end)/46 as cate10_cnt, -- cate10��¼��
	sum(case when cate = 11 then 1 else 0 end)/46 as cate11_cnt, -- cate11��¼��
	
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
	
	count(distinct case when cate = 6 and type = 1 then sku_id else null end)/46 as cate6_type1_sku_cnt, -- cate6��type1��Ʒ��
	count(distinct case when cate = 6 and type = 2 then sku_id else null end)/46 as cate6_type2_sku_cnt, -- cate6��type2��Ʒ��
	count(distinct case when cate = 6 and type = 3 then sku_id else null end)/46 as cate6_type3_sku_cnt, -- cate6��type3��Ʒ��
	count(distinct case when cate = 6 and type = 4 then sku_id else null end)/46 as cate6_type4_sku_cnt, -- cate6��type4��Ʒ��
	count(distinct case when cate = 6 and type = 5 then sku_id else null end)/46 as cate6_type5_sku_cnt, -- cate6��type5��Ʒ��
	count(distinct case when cate = 6 and type = 6 then sku_id else null end)/46 as cate6_type6_sku_cnt, -- cate6��type6��Ʒ��
    
	count(distinct case when cate = 5 and type = 1 then sku_id else null end)/46 as cate5_type1_sku_cnt, -- cate5��type1��Ʒ��
	count(distinct case when cate = 5 and type = 2 then sku_id else null end)/46 as cate5_type2_sku_cnt, -- cate5��type2��Ʒ��
	count(distinct case when cate = 5 and type = 3 then sku_id else null end)/46 as cate5_type3_sku_cnt, -- cate5��type3��Ʒ��
	count(distinct case when cate = 5 and type = 4 then sku_id else null end)/46 as cate5_type4_sku_cnt, -- cate5��type4��Ʒ��
	count(distinct case when cate = 5 and type = 5 then sku_id else null end)/46 as cate5_type5_sku_cnt, -- cate5��type5��Ʒ��
	count(distinct case when cate = 5 and type = 6 then sku_id else null end)/46 as cate5_type6_sku_cnt -- cate5��type6��Ʒ��
from
	dev.dev_ftp_cc_action_unique
where
	dt <= '2016-04-15'
group by
	user_id;

-- ��ȡ�ύѵ������
select
	A.*,
	B.sku_id,
	case when B.sku_id is not null then 1 else 0 end as label
from
	dev.dev_ftp_cc_online_train_x A
left outer join
	dev.dev_ftp_cc_online_train_y B
on
	A.user_id = B.user_id;
	
-- ��ȡ�ύԤ������
select * from dev.dev_ftp_cc_online_prediction_x;