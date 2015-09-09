-- Migration: flag-for-authorship
ALTER TABLE authorship ADD COLUMN writer_talent TEXT DEFAULT 'poop';
