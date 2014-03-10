DROP SCHEMA IF EXISTS `game`;
CREATE SCHEMA `game`;

DROP TABLE IF EXISTS `game`.`account`;
CREATE TABLE IF NOT EXISTS `game`.`account` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` char(16) NOT NULL DEFAULT '""',
  `type` tinyint(4) NOT NULL,
  CONSTRAINT pkey PRIMARY KEY(`id`),
  CONSTRAINT unique_name UNIQUE(`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

DROP TABLE IF EXISTS `game`.`account_to_role`; 
CREATE TABLE IF NOT EXISTS `game`.`account_to_role`(
  `account_id` bigint(20) NOT NULL,
  `role_id` bigint(20) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
  
DROP TABLE IF EXISTS `game`.`role`;
CREATE TABLE IF NOT EXISTS `game`.`role` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` char(16) NOT NULL,
  `level` int(11) NOT NULL DEFAULT '1',
  `sex` char(4) NOT NULL DEFAULT 'M',
  CONSTRAINT pkey PRIMARY KEY (`id`),
  CONSTRAINT unique_name UNIQUE(`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1;
