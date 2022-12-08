-- phpMyAdmin SQL Dump
-- version 4.7.7
-- https://www.phpmyadmin.net/
--
-- Host: localhost
-- Erstellungszeit: 12. Feb 2018 um 16:59
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
-- Datenbank: `MarbleOCU`
--

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `MarbleOCU`
--

CREATE TABLE `MarbleOCU` (
  `rt` text,
  `stimulus1` text,
  `stimulus2` text,
  `key_press` text,
  `old_order` text,
  `riskyKey` text,
  `new_order` text,
  `changed_Order` text,
  `red_marbles` text,
  `blue_marbles` text,
  `OtherChoseRisk` text,
  `ChooseRisk` text,
  `valueGamble` text,
  `probGamble` text,
  `Social1Ind0` text,
  `payoff` text,
  `cumulatedPayoff` text,
  `valueSure` text,
  `trialID` text,
  `PercentBlueEstimate` text,
  `HowSure` text,
  `test_part` text,
  `sex` text,
  `age` text,
  `subject` text
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
