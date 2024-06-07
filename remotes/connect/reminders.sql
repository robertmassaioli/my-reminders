--
-- PostgreSQL database dump
--

-- Dumped from database version 15.6 (Homebrew)
-- Dumped by pg_dump version 15.6 (Homebrew)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Data for Name: reminder; Type: TABLE DATA; Schema: public; Owner: myreminders
--

INSERT INTO public.reminder VALUES (4, 3, 11063, 'HT-12', 'My great new team', 'HT-12', 'My great new team', NULL, '2023-11-24 07:29:55.001+11', '557057:9eb9edaf-b755-4dd1-ab28-5c3f68948790', 0);
INSERT INTO public.reminder VALUES (5, 3, 11063, 'HT-12', 'My great new team', 'HT-12', 'My great new team', NULL, '2023-12-17 06:07:57.359+11', '557057:9eb9edaf-b755-4dd1-ab28-5c3f68948790', 0);
INSERT INTO public.reminder VALUES (6, 3, 11063, 'HT-12', 'My great new team', 'HT-12', 'My great new team', 'Did it work?', '2024-02-28 19:00:00+11', '557057:9eb9edaf-b755-4dd1-ab28-5c3f68948790', 0);


--
-- Name: reminder_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myreminders
--

SELECT pg_catalog.setval('public.reminder_id_seq', 6, true);


--
-- PostgreSQL database dump complete
--

