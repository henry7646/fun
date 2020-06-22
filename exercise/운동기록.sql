CREATE TABLE wh
(datetime date default sysdate primary key,
waist number(4,1),
hip number(4,1));

ALTER TABLE wh
ADD (weight number(3,1));

-- whr_update ���ν���: ��¥, �㸮�ѷ�, �����̵ѷ� �Է� �� 1)���� ���̺� �ش� ��¥�� WHR�� �� ��ȭ ��� �����ϸ� ��� 2)�׷��� ���� ��� �ش� ��¥�� WHR ��� 
CREATE OR REPLACE PROCEDURE whr_update
(whr_datetime in date,
whr_waist in number,
whr_hip in number,
whr_weight in number,
whr_result out varchar2,
whr_whr out number,
whr_bmi out number)
IS

cnt number := 0;

BEGIN

SELECT COUNT(*) INTO cnt
FROM wh
WHERE datetime = whr_datetime
AND ROWNUM = 1;

IF cnt > 0
THEN whr_result := '�̹� �����ϴ� ����Դϴ�';
SELECT ROUND(waist/hip,1) INTO whr_whr
FROM wh
WHERE datetime = whr_datetime;
SELECT ROUND(weight/POWER(1.69,2),1) INTO whr_bmi
FROM wh
WHERE datetime = whr_datetime;

ELSE
INSERT INTO wh(datetime,waist,hip,weight)
VALUES(whr_datetime, whr_waist, whr_hip, whr_weight);

whr_result := '����� �����߽��ϴ�';
SELECT ROUND(waist/hip,1) INTO whr_whr
FROM wh
WHERE datetime = whr_datetime;
SELECT ROUND(weight/POWER(1.69,2),1) INTO whr_bmi
FROM wh
WHERE datetime = whr_datetime;

END IF;

EXCEPTION
WHEN OTHERS THEN 
ROLLBACK;
whr_result := '���� �߻�';
whr_whr := NULL;
whr_bmi := NULL;

END;
-- ���ν��� ����

variable ��ȸ��� varchar2;
variable �㸮�����̺��� number;
variable ü�������� number;

EXECUTE whr_update(TO_DATE(SYSDATE,'YY/MM/DD'),100,106.5,82.6,:��ȸ���,:�㸮�����̺���,:ü��������);
PRINT ��ȸ���;
PRINT �㸮�����̺���;
PRINT ü��������;

-- � ���ۺ��� ���ݱ����� WHR�� �� ��ȭ ��Ÿ���� ��� (���� ���̺�)
DROP TABLE ����;
CREATE TABLE ���� AS
SELECT datetime, waist, hip, ROUND(waist/hip,1) AS whr,
DECODE(SIGN(ROUND(waist/hip,1) - LAG(ROUND(waist/hip,1)) OVER (ORDER BY datetime)), -1, 'IMPROVED',
0, 'SAME',
1, 'WORSENED') AS whr_improvement,
ABS(ROUND(waist/hip,1) - LAG(ROUND(waist/hip,1)) OVER (ORDER BY datetime)) AS whr_change,
weight,
ROUND(weight/POWER(1.69,2),1) AS bmi,
CASE WHEN ROUND(weight/POWER(1.69,2),1) < 18.5 THEN 'UNDERWEIGHT'
WHEN ROUND(weight/POWER(1.69,2),1) >= 18.5 AND ROUND(weight/POWER(1.69,2),1) < 25 THEN 'NORMAL'
WHEN ROUND(weight/POWER(1.69,2),1) >= 25 AND ROUND(weight/POWER(1.69,2),1) < 30 THEN 'OVERWEIGHT'
WHEN ROUND(weight/POWER(1.69,2),1) >= 30 THEN 'OBESE'
ELSE 'NA'
END AS obesity
FROM wh;

SELECT * FROM ����;