import React from 'react';
import { AnalysisResult, Datapoint } from '../types';
import { CheckCircle, AlertTriangle, Database, Code, Layout, Box, ArrowRight, Network } from 'lucide-react';

interface DatapointReviewProps {
    analysis: AnalysisResult;
    onConfirm: () => void;
    onBack: () => void;
}

const CategoryIcon = ({ category }: { category: string }) => {
    switch (category) {
        case 'ENTITY': return <Box className="w-5 h-5 text-blue-500" />;
        case 'DATABASE': return <Database className="w-5 h-5 text-green-500" />;
        case 'UI': return <Layout className="w-5 h-5 text-purple-500" />;
        case 'LOGIC': return <Code className="w-5 h-5 text-orange-500" />;
        case 'DEPENDENCY': return <Network className="w-5 h-5 text-red-500" />;
        default: return <CheckCircle className="w-5 h-5 text-slate-500" />;
    }
};

const DatapointReview: React.FC<DatapointReviewProps> = ({ analysis, onConfirm, onBack }) => {
    return (
        <div className="max-w-6xl mx-auto space-y-6 h-full flex flex-col">
            <div className="flex items-center justify-between">
                <div>
                    <h2 className="text-2xl font-bold text-slate-900">Analysis Review</h2>
                    <p className="text-slate-500">Confirm the extraction of business logic and entities before code generation.</p>
                </div>
                <div className="flex items-center gap-2 bg-white px-4 py-2 rounded-full border shadow-sm">
                    <span className="text-sm font-medium text-slate-600">Discovery Confidence:</span>
                    <span className="text-sm font-bold text-green-600">96%</span>
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 h-[600px]">
                {/* Program Info */}
                <div className="col-span-1 space-y-6 flex flex-col h-full">
                    <div className="bg-white p-6 rounded-xl border border-slate-200 shadow-sm">
                        <h3 className="text-xs uppercase tracking-wider font-bold text-slate-500 mb-4">Program Metadata</h3>
                        <div className="space-y-4">
                            <div>
                                <label className="text-xs text-slate-400">Program ID</label>
                                <div className="font-mono font-medium text-slate-900">{analysis.programId}</div>
                            </div>
                            <div>
                                <label className="text-xs text-slate-400">Complexity Score</label>
                                <div className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                                    analysis.complexity === 'HIGH' ? 'bg-red-100 text-red-800' :
                                    analysis.complexity === 'MEDIUM' ? 'bg-yellow-100 text-yellow-800' :
                                    'bg-green-100 text-green-800'
                                }`}>
                                    {analysis.complexity}
                                </div>
                            </div>
                            <div>
                                <label className="text-xs text-slate-400">Analysis Summary</label>
                                <p className="text-sm text-slate-700 mt-1 leading-relaxed">{analysis.summary}</p>
                            </div>
                        </div>
                    </div>

                    <div className="bg-indigo-50 p-6 rounded-xl border border-indigo-100 flex-1">
                        <h4 className="flex items-center gap-2 font-semibold text-indigo-900 mb-3">
                            <AlertTriangle className="w-4 h-4" />
                            Migration Strategy
                        </h4>
                        <p className="text-sm text-indigo-800 leading-relaxed mb-4">
                            The system has identified <strong className="font-semibold">{analysis.datapoints.length} key components</strong>.
                        </p>
                        <ul className="space-y-2 text-sm text-indigo-700">
                            <li className="flex items-center gap-2">
                                <div className="w-1.5 h-1.5 rounded-full bg-indigo-500"></div>
                                <span>Entities will be mapped to Domain Models.</span>
                            </li>
                            <li className="flex items-center gap-2">
                                <div className="w-1.5 h-1.5 rounded-full bg-indigo-500"></div>
                                <span>File descriptors will become DbContext/Repositories.</span>
                            </li>
                            <li className="flex items-center gap-2">
                                <div className="w-1.5 h-1.5 rounded-full bg-indigo-500"></div>
                                <span>Procedure logic will be extracted to Services.</span>
                            </li>
                        </ul>
                    </div>
                </div>

                {/* Datapoints List */}
                <div className="col-span-2 bg-slate-50 rounded-xl border border-slate-200 flex flex-col overflow-hidden">
                    <div className="p-4 border-b border-slate-200 bg-white">
                         <h3 className="text-xs uppercase tracking-wider font-bold text-slate-500">Identified Datapoints</h3>
                    </div>
                    <div className="flex-1 overflow-y-auto p-4 space-y-3 scrollbar-thin scrollbar-thumb-slate-300">
                        {analysis.datapoints.map((dp, idx) => (
                            <div key={idx} className="bg-white p-4 rounded-lg border border-slate-200 hover:border-indigo-400 hover:shadow-md transition-all group">
                                <div className="flex items-start justify-between mb-2">
                                    <div className="flex items-center gap-2">
                                        <CategoryIcon category={dp.category} />
                                        <span className="font-semibold text-slate-900">{dp.name}</span>
                                        <span className="text-[10px] uppercase bg-slate-100 px-2 py-0.5 rounded text-slate-500 font-bold tracking-wide">{dp.category}</span>
                                    </div>
                                    <span className="text-xs font-mono text-slate-400">
                                        {dp.confidence}%
                                    </span>
                                </div>
                                <p className="text-sm text-slate-600 mb-3">{dp.description}</p>
                                {dp.sourceLines && (
                                    <div className="bg-slate-900 rounded p-3 overflow-hidden relative">
                                        <div className="absolute top-0 right-0 px-2 py-1 bg-slate-800 text-[10px] text-slate-400 rounded-bl">COBOL Source</div>
                                        <pre className="text-xs font-mono text-slate-300 overflow-x-auto">
                                            <code>{dp.sourceLines}</code>
                                        </pre>
                                    </div>
                                )}
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="flex justify-end gap-4 pt-4 border-t border-slate-200">
                <button 
                    onClick={onBack}
                    className="px-6 py-2 text-sm font-medium text-slate-600 hover:text-slate-900"
                >
                    Back
                </button>
                <button 
                    onClick={onConfirm}
                    className="flex items-center gap-2 px-6 py-2 bg-indigo-600 text-white text-sm font-medium rounded-lg hover:bg-indigo-700 shadow-sm hover:shadow transition-all"
                >
                    Generate Modern Architecture
                    <ArrowRight className="w-4 h-4" />
                </button>
            </div>
        </div>
    );
};

export default DatapointReview;