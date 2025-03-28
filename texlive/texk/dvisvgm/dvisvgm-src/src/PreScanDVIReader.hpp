/*************************************************************************
** PreScanDVIReader.hpp                                                 **
**                                                                      **
** This file is part of dvisvgm -- a fast DVI to SVG converter          **
** Copyright (C) 2005-2025 Martin Gieseking <martin.gieseking@uos.de>   **
**                                                                      **
** This program is free software; you can redistribute it and/or        **
** modify it under the terms of the GNU General Public License as       **
** published by the Free Software Foundation; either version 3 of       **
** the License, or (at your option) any later version.                  **
**                                                                      **
** This program is distributed in the hope that it will be useful, but  **
** WITHOUT ANY WARRANTY; without even the implied warranty of           **
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the         **
** GNU General Public License for more details.                         **
**                                                                      **
** You should have received a copy of the GNU General Public License    **
** along with this program; if not, see <http://www.gnu.org/licenses/>. **
*************************************************************************/

#ifndef PRESCANDVIREADER_HPP
#define PRESCANDVIREADER_HPP

#include "BasicDVIReader.hpp"

struct DVIActions;

class PreScanDVIReader : public BasicDVIReader {
	public:
		PreScanDVIReader (std::istream &is, DVIActions *actions);
		unsigned currentPageNumber () const override {return _currentPageNumber;}

	protected:
		void cmdBop (int) override;
		void cmdXXX (int len) override;

	private:
		DVIActions *_actions;
		unsigned _currentPageNumber=0;
};

#endif
