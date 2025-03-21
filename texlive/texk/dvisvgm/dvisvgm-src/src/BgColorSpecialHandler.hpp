/*************************************************************************
** BgColorSpecialHandler.hpp                                            **
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

#ifndef BGCOLORSPECIALHANDLER_HPP
#define BGCOLORSPECIALHANDLER_HPP

#include <vector>
#include "Color.hpp"
#include "SpecialHandler.hpp"


class BgColorSpecialHandler : public SpecialHandler {
	public:
		void preprocess (const std::string &prefix, std::istream &is, SpecialActions &actions) override;
		bool process (const std::string &prefix, std::istream &is, SpecialActions &actions) override;
		const char* info () const override {return "background color special";}
		const char* name () const override {return handlerName();}
		static const char* handlerName ()  {return "bgcolor";}
		std::vector<const char*> prefixes () const override;

	protected:
		void dviBeginPage (unsigned pageno, SpecialActions &actions) override;

	private:
		using PageColor = std::pair<unsigned,Color>;  // page number and color
		std::vector<PageColor> _pageColors;
};

#endif
