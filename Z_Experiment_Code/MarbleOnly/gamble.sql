-- phpMyAdmin SQL Dump
-- version 4.7.7
-- https://www.phpmyadmin.net/
--
-- Host: localhost
-- Erstellungszeit: 12. Feb 2018 um 16:58
-- Server-Version: 10.1.30-MariaDB
-- PHP-Version: 7.1.13

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET AUTOCOMMIT = 0;
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Datenbank: `GambleOCU`
--

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `gamble`
--

CREATE TABLE `gamble` (
  `rt` text COLLATE utf8_unicode_ci,
  `stimulus1` text COLLATE utf8_unicode_ci,
  `stimulus2` text COLLATE utf8_unicode_ci,
  `key_press` text COLLATE utf8_unicode_ci,
  `old_order` text COLLATE utf8_unicode_ci,
  `new_order` text COLLATE utf8_unicode_ci,
  `changed_Order` text COLLATE utf8_unicode_ci,
  `riskyKey` text COLLATE utf8_unicode_ci,
  `ChooseRisk` text COLLATE utf8_unicode_ci,
  `probability` text COLLATE utf8_unicode_ci,
  `valueGamble` text COLLATE utf8_unicode_ci,
  `demoChoice` text COLLATE utf8_unicode_ci,
  `payoff` text COLLATE utf8_unicode_ci,
  `cumulatedPayoff` text COLLATE utf8_unicode_ci,
  `test_part` text COLLATE utf8_unicode_ci,
  `trial_type` text COLLATE utf8_unicode_ci,
  `trial_index` text COLLATE utf8_unicode_ci,
  `time_elapsed` text COLLATE utf8_unicode_ci,
  `internal_node_id` text COLLATE utf8_unicode_ci,
  `subject` text COLLATE utf8_unicode_ci,
  `sex` text COLLATE utf8_unicode_ci,
  `age` text COLLATE utf8_unicode_ci
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

--
-- Daten für Tabelle `gamble`
--
