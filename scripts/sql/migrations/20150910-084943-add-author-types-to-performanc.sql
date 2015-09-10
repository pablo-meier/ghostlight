-- Migration: add author types to performance


ALTER TYPE aggregated_performance ALTER ATTRIBUTE authors SET DATA TYPE authorship_agg[];
