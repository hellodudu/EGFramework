DROP SCHEMA IF EXISTS `game`;
CREATE SCHEMA `game`;

DROP TABLE IF EXISTS `game`.`account`;
CREATE TABLE IF NOT EXISTS `game`.`account` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) NOT NULL DEFAULT '""',
  `type` tinyint(4) NOT NULL,
  `role_id` bigint(20) NOT NULL,
  PRIMARY KEY (`id`,`role_id`),
  INDEX index_name(`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ;

DROP TABLE IF EXISTS `game`.`role`;
CREATE TABLE IF NOT EXISTS `game`.`role` (
  `id` bigint(20) NOT NULL,
  `name` varchar(256) NOT NULL,
  `level` int(11) NOT NULL DEFAULT '1',
  `sex` char(4) NOT NULL DEFAULT 'M',
  PRIMARY KEY (`id`),
  INDEX index_name(`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
COMMIT;
