CREATE TABLE EVENTS(
    ID BIGINT AUTO_INCREMENT,
    PAYLOAD CLOB,
    TIME_STAMP TIMESTAMP AS CURRENT_TIMESTAMP()
);

CREATE TABLE MARKETPLACE(
    PARTNER VARCHAR(255) UNIQUE,
    URL VARCHAR(255)
);

CREATE TABLE COMPANY(
    UUID VARCHAR(255) UNIQUE,
    EMAIL VARCHAR(255),
    PHONE_NUMBER VARCHAR(20),
    NAME VARCHAR(255),
    WEBSITE VARCHAR(255)
);

CREATE TABLE CREATOR(
    UUID VARCHAR(255) UNIQUE,
    OPENID VARCHAR(255) UNIQUE,
    FIRST_NAME VARCHAR(255),
    LAST_NAME VARCHAR(255),
    EMAIL VARCHAR(255),
    COMPANY VARCHAR,
    MARKETPLACE VARCHAR(255)
);

CREATE TABLE SUBSCRIPTIONS(
    ACCOUNT_IDENTIFIER BIGINT AUTO_INCREMENT,
    MARKETPLACE VARCHAR(255),
    CREATOR VARCHAR(255),
    COMPANY VARCHAR(255),
    STATUS VARCHAR(255),
    NUM_USERS INT,
    NUM_MEGABYTES INT,
    EDITION_CODE VARCHAR(255)
);

