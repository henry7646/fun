CREATE TABLE wh
(datetime date default sysdate primary key,
waist number(4,1),
hip number(4,1));

ALTER TABLE wh
ADD (weight number(3,1));

-- whr_update 프로시져: 날짜, 허리둘레, 엉덩이둘레 입력 시 1)운동기록 테이블에 해당 날짜의 WHR과 그 변화 기록 존재하면 출력 2)그렇지 않을 경우 해당 날짜의 WHR 출력 
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
THEN whr_result := '이미 존재하는 기록입니다';
SELECT ROUND(waist/hip,1) INTO whr_whr
FROM wh
WHERE datetime = whr_datetime;
SELECT ROUND(weight/POWER(1.69,2),1) INTO whr_bmi
FROM wh
WHERE datetime = whr_datetime;

ELSE
INSERT INTO wh(datetime,waist,hip,weight)
VALUES(whr_datetime, whr_waist, whr_hip, whr_weight);

whr_result := '기록을 갱신했습니다';
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
whr_result := '오류 발생';
whr_whr := NULL;
whr_bmi := NULL;

END;
-- 프로시져 종료

variable 조회결과 varchar2;
variable 허리엉덩이비율 number;
variable 체질량지수 number;

EXECUTE whr_update(TO_DATE(SYSDATE,'YY/MM/DD'),100,106.5,82.6,:조회결과,:허리엉덩이비율,:체질량지수);
PRINT 조회결과;
PRINT 허리엉덩이비율;
PRINT 체질량지수;

-- 운동 시작부터 지금까지의 WHR과 그 변화 나타내는 기록 (운동기록 테이블)
DROP TABLE 운동기록;
CREATE TABLE 운동기록 AS
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

SELECT * FROM 운동기록;